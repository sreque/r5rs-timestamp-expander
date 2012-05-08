(module schelog-test racket
  (provide schelog-test)
  (require racket 
           rackunit
           "../clinger-rees-syntax-rules.rkt"
           "../clinger-rees-parser.rkt"
           "../clinger-rees-env.rkt"
           "test-utils.rkt")

  (define program '(
;MzScheme version of
;schelog.scm
;Schelog
;An embedding of Prolog in Scheme
;Dorai Sitaram
;1989, revised Feb. 1993, Mar. 1997

;logic variables and their manipulation

(define schelog:*ref* "ref")

(define schelog:*unbound* '_)

(define schelog:make-ref
  ;;makes a fresh unbound ref;
  ;;unbound refs point to themselves
  (lambda opt
    (vector schelog:*ref*
      (if (null? opt) schelog:*unbound*
	(car opt)))))

(define _ schelog:make-ref)

(define schelog:ref?
  (lambda (r)
    (and (vector? r)
	 (eq? (vector-ref r 0) schelog:*ref*))))

(define schelog:deref
  (lambda (r)
    (vector-ref r 1)))

(define schelog:set-ref!
  (lambda (r v)
    (vector-set! r 1 v)))

(define schelog:unbound-ref?
  (lambda (r)
    (eq? (schelog:deref r) schelog:*unbound*)))

(define schelog:unbind-ref!
  (lambda (r)
    (schelog:set-ref! r schelog:*unbound*)))

;frozen logic vars

(define schelog:*frozen* "frozen")

(define schelog:freeze-ref
  (lambda (r)
    (schelog:make-ref (vector schelog:*frozen* r))))

(define schelog:thaw-frozen-ref
  (lambda (r)
    (vector-ref (schelog:deref r) 1)))

(define schelog:frozen-ref?
  (lambda (r)
    (let ((r2 (schelog:deref r)))
      (and (vector? r2)
	   (eq? (vector-ref r2 0) schelog:*frozen*)))))

;deref a structure completely (except the frozen ones, i.e.)

(define schelog:deref*
  (lambda (s)
    (cond ((schelog:ref? s)
	   (if (schelog:frozen-ref? s) s
	     (schelog:deref* (schelog:deref s))))
	  ((pair? s) (cons (schelog:deref* (car s))
                       (schelog:deref* (cdr s))))
	  ((vector? s)
	   (list->vector (map schelog:deref* (vector->list s))))
	  (else s))))

;%let introduces new logic variables

(define-syntax %let
  (syntax-rules ()
    ((%let (x ...) . e)
      (let ((x (schelog:make-ref)) ...)
        . e))))

;the unify predicate

(define *schelog-use-occurs-check?* #f)

(define schelog:occurs-in? 
  (lambda (var term)
    (and *schelog-use-occurs-check?*
         (let loop ((term term))
           (cond ((eqv? var term) #t)
                 ((schelog:ref? term)
                  (cond ((schelog:unbound-ref? term) #f)
                        ((schelog:frozen-ref? term) #f)
                        (else (loop (schelog:deref term)))))
                 ((pair? term)
                  (or (loop (car term)) (loop (cdr term))))
                 ((vector? term)
                  (loop (vector->list term)))
                 (else #f))))))

(define schelog:unify
  (lambda (t1 t2)
    (lambda (fk)
      (letrec
        ((cleanup-n-fail
           (lambda (s)
             (for-each schelog:unbind-ref! s)
             (fk 'fail)))
         (unify1
           (lambda (t1 t2 s)
             ;(printf "unify1 ~s ~s~%" t1 t2)
             (cond ((eqv? t1 t2) s)
                   ((schelog:ref? t1)
                    (cond ((schelog:unbound-ref? t1)
                           (cond ((schelog:occurs-in? t1 t2)
                                  (cleanup-n-fail s))
                                 (else 
                                   (schelog:set-ref! t1 t2)
                                   (cons t1 s))))
                          ((schelog:frozen-ref? t1)
                           (cond ((schelog:ref? t2)
                                  (cond ((schelog:unbound-ref? t2)
                                         ;(printf "t2 is unbound~%")
                                         (unify1 t2 t1 s))
                                        ((schelog:frozen-ref? t2)
                                         (cleanup-n-fail s))
                                        (else
                                          (unify1 t1 (schelog:deref t2) s))))
                                 (else (cleanup-n-fail s))))
                          (else 
                            ;(printf "derefing t1~%") 
                            (unify1 (schelog:deref t1) t2 s))))
                   ((schelog:ref? t2) (unify1 t2 t1 s))
                   ((and (pair? t1) (pair? t2))
                    (unify1 (cdr t1) (cdr t2)
                            (unify1 (car t1) (car t2) s)))
                   ((and (string? t1) (string? t2))
                    (if (string=? t1 t2) s
                        (cleanup-n-fail s)))
                   ((and (vector? t1) (vector? t2))
                    (unify1 (vector->list t1)
                            (vector->list t2) s))
                   (else
                     (for-each schelog:unbind-ref! s)
                     (fk 'fail))))))
        (let ((s (unify1 t1 t2 '())))
          (lambda (d)
            (cleanup-n-fail s)))))))

(define %= schelog:unify)

;disjunction

(define-syntax %or
  (syntax-rules ()
    ((%or g ...)
     (lambda (__fk)
       (call-with-current-continuation
	 (lambda (__sk)
	   (call-with-current-continuation
	     (lambda (__fk)
	       (__sk ((schelog:deref* g) __fk))))
	   ...
	   (__fk 'fail)))))))

;conjunction

(define-syntax %and
  (syntax-rules ()
    ((%and g ...)
     (lambda (__fk)
       (let* ((__fk ((schelog:deref* g) __fk))
	      ...)
	 __fk)))))

;cut

(define-syntax %cut-delimiter
  (syntax-rules (!)
    ((%cut-delimiter g)
     (lambda (__fk)
       (let ((! (lambda (__fk2) __fk)))
	 ((schelog:deref* g) __fk))))))

;Prolog-like sugar

(define-syntax %rel
  (syntax-rules (!)
    ((%rel (v ...) ((a ...) subgoal ...) ...)
      (lambda __fmls
        (lambda (__fk)
          (call-with-current-continuation
            (lambda (__sk)
              (let ((! (lambda (fk1) __fk)))
                (%let (v ...)
                  (call-with-current-continuation
                    (lambda (__fk)
                      (let* ((__fk ((%= __fmls (list a ...)) __fk))
                              (__fk ((schelog:deref* subgoal) __fk))
                              ...)
                        (__sk __fk))))
                  ...
                  (__fk 'fail))))))))))

;the fail and true preds

(define %fail
  (lambda (fk) (fk 'fail)))

(define %true
  (lambda (fk) fk))

;for structures ("functors"), use Scheme's list and vector
;functions and anything that's built using them.

;arithmetic

(define-syntax %is
  (syntax-rules (quote)
    ((%is v e)
     (lambda (__fk)
       ((%= v (%is (1) e __fk)) __fk)))

    ((%is (1) (quote x) fk) (quote x))
    ((%is (1) (x ...) fk)
     ((%is (1) x fk) ...))
    ((%is (1) x fk)
     (if (and (schelog:ref? x) (schelog:unbound-ref? x))
	 (fk 'fail) (schelog:deref* x)))))

;defining arithmetic comparison operators

(define schelog:make-binary-arithmetic-relation
  (lambda (f)
    (lambda (x y)
      (%is #t (f x y)))))

(define %=:= (schelog:make-binary-arithmetic-relation =))
(define %> (schelog:make-binary-arithmetic-relation >))
(define %>= (schelog:make-binary-arithmetic-relation >=))
(define %< (schelog:make-binary-arithmetic-relation <))
(define %<= (schelog:make-binary-arithmetic-relation <=))
(define %=/= (schelog:make-binary-arithmetic-relation
               (lambda (m n) (not (= m n)))))

;type predicates

(define schelog:constant?
  (lambda (x)
    (cond ((schelog:ref? x)
	   (cond ((schelog:unbound-ref? x) #f)
		 ((schelog:frozen-ref? x) #t)
		 (else (schelog:constant? (schelog:deref x)))))
	  ((pair? x) #f)
	  ((vector? x) #f)
	  (else #t))))

(define schelog:compound?
  (lambda (x)
    (cond ((schelog:ref? x) (cond ((schelog:unbound-ref? x) #f)
			  ((schelog:frozen-ref? x) #f)
			  (else (schelog:compound? (schelog:deref x)))))
	  ((pair? x) #t)
	  ((vector? x) #t)
	  (else #f))))

(define %constant
  (lambda (x)
    (lambda (fk)
      (if (schelog:constant? x) fk (fk 'fail)))))

(define %compound
  (lambda (x)
    (lambda (fk)
      (if (schelog:compound? x) fk (fk 'fail)))))

;metalogical type predicates

(define schelog:var?
  (lambda (x)
    (cond ((schelog:ref? x)
	   (cond ((schelog:unbound-ref? x) #t)
		 ((schelog:frozen-ref? x) #f)
		 (else (schelog:var? (schelog:deref x)))))
	  ((pair? x) (or (schelog:var? (car x)) (schelog:var? (cdr x))))
	  ((vector? x) (schelog:var? (vector->list x)))
	  (else #f))))

(define %var
  (lambda (x)
    (lambda (fk) (if (schelog:var? x) fk (fk 'fail)))))

(define %nonvar
  (lambda (x)
    (lambda (fk) (if (schelog:var? x) (fk 'fail) fk))))

; negation of unify

(define schelog:make-negation ;basically inlined cut-fail
  (lambda (p)
    (lambda args
      (lambda (fk)
	(if (call-with-current-continuation
	      (lambda (k)
		((apply p args) (lambda (d) (k #f)))))
	    (fk 'fail)
	    fk)))))

(define %/=
  (schelog:make-negation %=))

;identical

(define schelog:ident?
  (lambda (x y)
    (cond ((schelog:ref? x)
	   (cond ((schelog:unbound-ref? x)
		  (cond ((schelog:ref? y)
			 (cond ((schelog:unbound-ref? y) (eq? x y))
			       ((schelog:frozen-ref? y) #f)
			       (else (schelog:ident? x (schelog:deref y)))))
			(else #f)))
		 ((schelog:frozen-ref? x)
		  (cond ((schelog:ref? y)
			 (cond ((schelog:unbound-ref? y) #f)
			       ((schelog:frozen-ref? y) (eq? x y))
			       (else (schelog:ident? x (schelog:deref y)))))
			(else #f)))
		 (else (schelog:ident? (schelog:deref x) y))))
	  ((pair? x)
	   (cond ((schelog:ref? y)
		  (cond ((schelog:unbound-ref? y) #f)
			((schelog:frozen-ref? y) #f)
			(else (schelog:ident? x (schelog:deref y)))))
		 ((pair? y)
		  (and (schelog:ident? (car x) (car y))
		       (schelog:ident? (cdr x) (cdr y))))
		 (else #f)))
	  ((vector? x)
	   (cond ((schelog:ref? y)
		  (cond ((schelog:unbound-ref? y) #f)
			((schelog:frozen-ref? y) #f)
			(else (schelog:ident? x (schelog:deref y)))))
		 ((vector? y)
		  (schelog:ident? (vector->list x)
		    (vector->list y)))
		 (else #f)))
	  (else
	    (cond ((schelog:ref? y)
		   (cond ((schelog:unbound-ref? y) #f)
			 ((schelog:frozen-ref? y) #f)
			 (else (schelog:ident? x (schelog:deref y)))))
		  ((pair? y) #f)
		  ((vector? y) #f)
		  (else (eqv? x y)))))))

(define %==
  (lambda (x y)
    (lambda (fk) (if (schelog:ident? x y) fk (fk 'fail)))))

(define %/==
  (lambda (x y)
    (lambda (fk) (if (schelog:ident? x y) (fk 'fail) fk))))

;variables as objects

(define schelog:freeze
  (lambda (s)
    (let ((dict '()))
      (let loop ((s s))
	(cond ((schelog:ref? s)
	       (cond ((or (schelog:unbound-ref? s) (schelog:frozen-ref? s))
		      (let ((x (assq s dict)))
			(if x (cdr x)
			    (let ((y (schelog:freeze-ref s)))
			      (set! dict (cons (cons s y) dict))
			      y))))
		     ;((schelog:frozen-ref? s) s) ;?
		     (else (loop (schelog:deref s)))))
	      ((pair? s) (cons (loop (car s)) (loop (cdr s))))
	      ((vector? s)
	       (list->vector (map loop (vector->list s))))
	      (else s))))))

(define schelog:melt
  (lambda (f)
    (cond ((schelog:ref? f)
	   (cond ((schelog:unbound-ref? f) f)
		 ((schelog:frozen-ref? f) (schelog:thaw-frozen-ref f))
		 (else (schelog:melt (schelog:deref f)))))
	  ((pair? f)
	   (cons (schelog:melt (car f)) (schelog:melt (cdr f))))
	  ((vector? f)
	   (list->vector (map schelog:melt (vector->list f))))
	  (else f))))

(define schelog:melt-new
  (lambda (f)
    (let ((dict '()))
      (let loop ((f f))
	(cond ((schelog:ref? f)
	       (cond ((schelog:unbound-ref? f) f)
		     ((schelog:frozen-ref? f)
		      (let ((x (assq f dict)))
			(if x (cdr x)
			    (let ((y (schelog:make-ref)))
			      (set! dict (cons (cons f y) dict))
			      y))))
		     (else (loop (schelog:deref f)))))
	      ((pair? f) (cons (loop (car f)) (loop (cdr f))))
	      ((vector? f)
	       (list->vector (map loop (vector->list f))))
	      (else f))))))

(define schelog:copy
  (lambda (s)
    (schelog:melt-new (schelog:freeze s))))

(define %freeze
  (lambda (s f)
    (lambda (fk)
      ((%= (schelog:freeze s) f) fk))))

(define %melt
  (lambda (f s)
    (lambda (fk)
      ((%= (schelog:melt f) s) fk))))

(define %melt-new
  (lambda (f s)
    (lambda (fk)
      ((%= (schelog:melt-new f) s) fk))))

(define %copy
  (lambda (s c)
    (lambda (fk)
      ((%= (schelog:copy s) c) fk))))

;negation as failure

(define %not
  (lambda (g)
    (lambda (fk)
      (if (call-with-current-continuation
	    (lambda (k)
	      ((schelog:deref* g) (lambda (d) (k #f)))))
	  (fk 'fail) fk))))

;assert, asserta

(define %empty-rel
  (lambda args
    %fail))

(define-syntax %assert
  (syntax-rules (!)
    ((%assert rel-name (v ...) ((a ...) subgoal ...) ...)
      (set! rel-name
        (let ((__old-rel rel-name)
               (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
          (lambda __fmls
            (%or (apply __old-rel __fmls)
              (apply __new-addition __fmls))))))))

(define-syntax %assert-a
  (syntax-rules (!)
    ((%assert-a rel-name (v ...) ((a ...) subgoal ...) ...)
      (set! rel-name
        (let ((__old-rel rel-name)
               (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
          (lambda __fmls
            (%or (apply __new-addition __fmls)
              (apply __old-rel __fmls))))))))

;set predicates

(define schelog:set-cons
  (lambda (e s)
    (if (member e s) s (cons e s))))

(define-syntax %free-vars
  (syntax-rules ()
    ((%free-vars (v ...) g)
      (cons 'schelog:goal-with-free-vars
        (cons (list v ...) g)))))

(define schelog:goal-with-free-vars?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'schelog:goal-with-free-vars))))

(define schelog:make-bag-of
  (lambda (kons)
    (lambda (lv goal bag)
      (let ((fvv '()))
        (when (schelog:goal-with-free-vars? goal)
          (begin (set! fvv (cadr goal))
            (set! goal (cddr goal))))
        (schelog:make-bag-of-aux kons fvv lv goal bag)))))

(define schelog:make-bag-of-aux
  (lambda (kons fvv lv goal bag)
    (lambda (fk)
      (call-with-current-continuation
        (lambda (sk)
          (let ((lv2 (cons fvv lv)))
            (let* ((acc '())
                    (fk-final
                      (lambda (d)
                        ;;(set! acc (reverse! acc))
                        (sk ((schelog:separate-bags fvv bag acc) fk))))
                    (fk-retry (goal fk-final)))
              (set! acc (kons (schelog:deref* lv2) acc))
              (fk-retry 'retry))))))))

(define schelog:separate-bags
  (lambda (fvv bag acc)
    ;;(format #t "Accum: ~s~%" acc)
    (let ((bags (let loop ((acc acc)
                            (current-fvv #f) (current-bag '())
                            (bags '()))
                  (if (null? acc)
                    (cons (cons current-fvv current-bag) bags)
                    (let ((x (car acc)))
                      (let ((x-fvv (car x)) (x-lv (cdr x)))
                        (if (or (not current-fvv) (equal? x-fvv current-fvv))
                          (loop (cdr acc) x-fvv (cons x-lv current-bag) bags)
                          (loop (cdr acc) x-fvv (list x-lv)
                            (cons (cons current-fvv current-bag) bags)))))))))
      ;;(format #t "Bags: ~a~%" bags)
      (if (null? bags) (%= bag '())
        (let ((fvv-bag (cons fvv bag)))
          (let loop ((bags bags))
            (if (null? bags) %fail
              (%or (%= fvv-bag (car bags))
                (loop (cdr bags))))))))))

(define %bag-of (schelog:make-bag-of cons))
(define %set-of (schelog:make-bag-of schelog:set-cons))

;%bag-of-1, %set-of-1 hold if there's at least one solution

(define %bag-of-1
  (lambda (x g b)
    (%and (%bag-of x g b)
      (%= b (cons (_) (_))))))

(define %set-of-1
  (lambda (x g s)
    (%and (%set-of x g s)
      (%= s (cons (_) (_))))))

;user interface

;(%which (v ...) query) returns #f if query fails and instantiations
;of v ... if query succeeds.  In the latter case, type (%more) to
;retry query for more instantiations.

(define schelog:*more-k* 'forward)
(define schelog:*more-fk* 'forward)

(define-syntax %which
  (syntax-rules (schelog:*more-k* schelog:*more-fk*)
    ((%which (v ...) g)
     (%let (v ...)
       (call-with-current-continuation
         (lambda (__qk)
           (set! schelog:*more-k* __qk)
           (set! schelog:*more-fk*
             ((schelog:deref* g)
              (lambda (d)
                (set! schelog:*more-fk* #f)
                (schelog:*more-k* #f))))
           (schelog:*more-k*
             (map (lambda (nam val) (list nam (schelog:deref* val)))
                  '(v ...)
                  (list v ...)))))))))

(define %more
  (lambda ()
    (call-with-current-continuation
      (lambda (k)
	(set! schelog:*more-k* k)
	(if schelog:*more-fk* (schelog:*more-fk* 'more)
	  #f)))))

;end of embedding code.  The following are
;some utilities, written in Schelog

(define %member
  (lambda (x y)
    (%let (xs z zs)
      (%or
	(%= y (cons x xs))
	(%and (%= y (cons z zs))
	  (%member x zs))))))

;this is broken because ! isn't defined
(define %if-then-else
  (lambda (p q r)
    (%cut-delimiter
      (%or
	(%and p ! q)
	r))))

(define %append
  (%rel (x xs ys zs)
    (('() ys ys))
    (((cons x xs) ys (cons x zs))
      (%append xs ys zs))))

(define %repeat
  ;;failure-driven loop
  (%rel ()
    (())
    (() (%repeat))))

; deprecated names -- retained here for backward-compatibility

(define == %=)
(define %notunify %/=)

(define %eq %=:=)
(define %gt %>)
(define %ge %>=)
(define %lt %<)
(define %le %<=)
(define %ne %=/=)
(define %ident %==)
(define %notident %/==)
(define-syntax %exists (syntax-rules () ((%exists vv g) g)))

(define more %more)

;end of implementation

;Test

;map coloring, example from Sterling & Shapiro, p. 212

;(%members x y) holds if x is a subset of y

(define %members
  (%rel (X Xs Ys)
    (((cons X Xs) Ys) (%member X Ys) (%members Xs Ys))
    (('() Ys))))

;(%select x y z) holds if z is y with one less occurrence of x

(define %select
  (%rel (X Xs Y Ys Zs)
    ((X (cons X Xs) Xs))
    ((X (cons Y Ys) (cons Y Zs))
     (%select X Ys Zs))))

;region is a structure-builder

(define region
  (lambda (name color neighbors)
    (list 'region name color neighbors)))

(define %color-map
  (%rel (Region Regions Colors)
    (((cons Region Regions) Colors)
     (%color-region Region Colors) (%color-map Regions Colors))
    (('() Colors))))

(define %color-region
  (%rel (Name Color Neighbors Colors Colors1)
    (((region Name Color Neighbors) Colors)
     (%select Color Colors Colors1)
     (%members Neighbors Colors1))))

(define %test-color
  (%rel (Name Map Colors)
    ((Name Map)
     (%map Name Map)
     (%colors Colors)
     (%color-map Map Colors))))

(define %map
  (%rel (A B C D E F G H I L P S)
    (('test (list
	      (region 'a A (list B C D))
	      (region 'b B (list A C E))
	      (region 'c C (list A B D E F))
	      (region 'd D (list A C F))
	      (region 'e E (list B C F))
	      (region 'f F (list C D E)))))
    (('western-europe
       (list
	 (region 'portugal P (list E))
	 (region 'spain E (list F P))
	 (region 'france F (list E I S B G L))
	 (region 'belgium B (list F H L G))
	 (region 'holland H (list B G))
	 (region 'germany G (list F A S H B L))
	 (region 'luxembourg L (list F B G))
	 (region 'italy I (list F A S))
	 (region 'switzerland S (list F I A G))
	 (region 'austria A (list I S G)))))))

(define %colors
  (%rel ()
    (('(red yellow blue white)))))
))

  (make-expand-test-defs)
  
  (define schelog-test 
    (test-suite
     "Schelog test"     
     (expand-and-eval program)
     (test-expand-and-eval
      (%which (M) (%test-color 'test M))
      ((M
        ((region a red (yellow blue yellow))
         (region b yellow (red blue red))
         (region c blue (red yellow yellow red white))
         (region d yellow (red blue white))
         (region e red (yellow blue white))
         (region f white (blue yellow red))))))     
     (test-expand-and-eval
      (%which (M) (%test-color 'western-europe M))
      ((M
        ((region portugal red (yellow))
         (region spain yellow (red red))
         (region france red (yellow yellow blue yellow white blue))
         (region belgium yellow (red red blue white))
         (region holland red (yellow white))
         (region germany white (red red blue red yellow blue))
         (region luxembourg blue (red yellow white))
         (region italy yellow (red red blue))
         (region switzerland blue (red yellow red white))
         (region austria red (yellow blue white))))))
     )))

