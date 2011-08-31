#lang racket
(require racket rackunit
         "clinger-rees-syntax-rules.rkt")
(define (check-match-success result)
  (check-pred hash? result))

(define (check-match-failure result)
  (pattern-mismatch? result))

(define hash-empty? (compose not hash-iterate-first))

;test matching of various pattern data (characters, booleans, numbers, strings)
(let* ([syntax 'a]
      [p1 (datum 'a)] ;really symbols shouldn't be supported
      [p2 (datum "a")]
      [p3 (datum #\a)]
      [p4 (fixed-list '() (list (datum syntax)))])
  (define result ((make-matcher p1 (hash)) syntax (hash)))
  (check-match-success result)
  (check-pred hash-empty? result (format "hash should be empty: ~a\n" result))
  (for ([p (list p2 p3 p4)])
    (check-match-failure ((make-matcher p (hash)) syntax (hash)))))

;test pattern identifier's resulting matcher matching different kinds of syntax.
(let ([pattern (pattern-identifier 'v)]
      [s1 'v]
      [s2 '(a b c d)]
      [s3 #(bob dole is cool)]
      [s4 #hash( (1 . 2) (3 . 4) (5 . 6))])
  (for ([s (list s1 s2 s3 s4)])
    (define result ((make-matcher pattern (hash)) s (hash)))
    (check-match-success result)
    (check-equal? result (hash (input-pattern-source pattern) s))))

;test matching of a  literal identifier based on the environment
(let* ([id 'x]
       [pattern (literal-identifier id)]
       [bad-syntaxes (list 'a "b" #\x `(,id) #(x))]
       [empty-env (hash)]
       [env1 (hash id (gensym id))]
       [env2 (hash id (gensym id))]
       [id2 'y]
       [diff-env (hash id2 (hash-ref env1 id))])
  (for ([env (list empty-env env1 env2)])
    (define result ((make-matcher pattern env) id env))
    (check-match-success result)
    (check-pred hash-empty? result))
  (for ([s bad-syntaxes])
    (check-match-failure ((make-matcher pattern empty-env) s empty-env)))
  (for ([e1 (list empty-env env1 env2)]
        [e2 (list env1 env2 empty-env)])
    (check-match-failure ((make-matcher pattern e1) id e2)))
  (check-match-success ((make-matcher pattern env1) id2 diff-env))
  (check-match-failure ((make-matcher pattern env1) id diff-env))
  (check-match-failure ((make-matcher pattern env2) id2 diff-env)))

;test matching of fixed list against syntax lists of the same and different sizes and with different contents
(let* ([syntax '(a b c d e f g)]
       [big-syntax (append syntax '(h))]
       [small-syntax (take syntax (sub1 (length syntax)))]
       [wrong-syntax (map (lambda (s) (if (eqv? s 'd) 'z s)) syntax)]
       [pattern (fixed-list syntax (map (lambda (s) (datum s)) syntax))]
       [matcher (make-matcher pattern (hash))])
  (define good-result (matcher syntax (hash)))
  (check-match-success good-result)
  (check-pred hash-empty? good-result)
  (check-match-failure (matcher big-syntax (hash)))
  (check-match-failure (matcher wrong-syntax (hash))))

;check matching against nested fixed lists
(let* ([syntax '((invoke return value) literal (a b))]
       [id1 'x]
       [id2 'y]
       [id3 'z]
       [pattern (fixed-list '() 
                            (list (pattern-identifier id1) 
                                  (literal-identifier 'literal)
                                  (fixed-list '() (list (pattern-identifier id2)
                                                        (pattern-identifier id3)))))])
  (define result ((make-matcher pattern (hash)) syntax (hash)))
  (check-equal? result (hash id1 '(invoke return value) id2 'a id3 'b)))

;This test is flawed and needs to be changed or removed. 
;datum should not be used to match symbols. 
(let* ([syntax '(a b c (d e (f g h) i) j k)]
       [id1 'x]
       [id2 'y]
      [pattern (improper-list 
                '() 
                (list (datum 'a) (datum 'b) (pattern-identifier id1))
                (fixed-list 
                 '()
                 (list
                  (fixed-list
                   '()
                   (list (datum 'd) 
                         (datum 'e)
                         (pattern-identifier id2)
                         (datum 'i)))
                  (datum 'j)
                  (datum 'k))))]
      [matcher (make-matcher pattern (hash))]
      [too-short '(a b)]
      [too-long (cons 'z syntax)])
  (define result (matcher syntax (hash)))
  (check-equal? result (hash id1 'c id2 '(f g h)))
  (check-match-failure (matcher too-short (hash)))
  (check-match-failure (matcher too-long (hash)))
  (check-match-failure (matcher 'weird-syntax (hash))))

;test matching of ellipses list against lists of varying length
(let* ([syntax '(a b c d e f g)]
       [id 'x]
       [id2 'y]
       [pattern (ellipses-list '() 
                               (list (datum 'a) (datum 'b) (pattern-identifier id2)) 
                               (pattern-identifier id))]
       [too-small '(a b c)]
       [really-small '(a)]
       [just-right '(a b c d)]
       [matcher (curryr (make-matcher pattern (hash)) (hash))])
  (check-equal? (matcher syntax) (hash id '(d e f g) id2 'c))
  (check-equal? (matcher just-right) (hash id '(d) id2 'c))
  (check-match-failure (matcher too-small))
  (check-match-failure (matcher really-small))
  (check-match-failure (matcher 5)))

;test matching of a ((x ...) ...) against a nested list
(let* ([syntax '((a b c) (c d e) (f g h) (i j k) (l m) (n))]
       [pattern (ellipses-list '() (list) (ellipses-list '() (list) (pattern-identifier 'x)))])
  (check-equal? ((make-matcher pattern (hash)) syntax (hash)) (hash 'x syntax)))

;test parsing of an ellipses list and resulting matcher.
(let*
    ([pattern (parse-transformer-pattern '(test1 test2 ...) (set))]
     [ids (compute-ellipses-nesting pattern)])
  (check-pred ellipses-list? pattern)
  (check-equal? (ellipses-list-sub-patterns pattern) (list (pattern-identifier 'test1)))
  (check-equal? (ellipses-list-tail-pattern pattern) (pattern-identifier 'test2))
  (check-equal? ids (hash 'test1 0 'test2 1)))

;test parsing of 3-level deep ellipses pattern and resulting matcher
(let*
    ([pattern 
      (parse-transformer-pattern 
       '(((((a ...) (b ...) (c ...)) ...) -) ...) (set))]
     [ids (compute-ellipses-nesting pattern)])
  (check-equal? 
   pattern
   (ellipses-list
    '(((((a ...) (b ...) (c ...)) ...) -) ...) '()
    (fixed-list 
     '((((a ...) (b ...) (c ...)) ...) -)
     (list
      (ellipses-list
       '(((a ...) (b ...) (c ...)) ...) '()
       (fixed-list
        '((a ...) (b ...) (c ...))
        (list
         (ellipses-list
          '(a ...) '()
          (pattern-identifier 'a))
         (ellipses-list
          '(b ...) '()
          (pattern-identifier 'b))
         (ellipses-list
          '(c ...) '()
          (pattern-identifier 'c)))))
      (pattern-identifier '-)))) )              
  (check-equal? ids (hash 'a 3 'b 3 'c 3 '- 1)))

;checks that duplicate ids lead to syntax error
(let ([pattern 
       (parse-transformer-pattern 
        '((((a ...) (b ...)) ...) (d e f g . ((h (i j (k l m (a)))) ...))) (set))])
  (check-exn syntax-error? (lambda () (compute-ellipses-nesting pattern)))
  )

;basic ellipses nesting test
(let* ([pattern
        (parse-transformer-pattern
         '(a b |.| (d #t 5 "bob" #\c e)) (set 'd 'e))]
       [ids (compute-ellipses-nesting pattern)])
  (check-equal?
   pattern
   (improper-list
    '(a b |.| (d #t 5 "bob" #\c e))
    (list
     (pattern-identifier 'a)
     (pattern-identifier 'b))
    (fixed-list
     '(d #t 5 "bob" #\c e)
     (list
      (literal-identifier 'd)
      (datum #t)
      (datum 5)
      (datum "bob")
      (datum #\c)
      (literal-identifier 'e)))))
  (check-equal? ids (hash 'a 0 'b 0)))


;test parsing of ellipses template
(let*
    ([template
      (parse-transformer-template
       '((a ...))
       (set 'a))])
  (check-equal?
   template
   (template-list 
    '((a ...))
    (list 
     (template-list
      '(a ...)
      (list
       (ellipses-template
        'a
        (template-identifier 'a)
        1
        (set 'a))))))))

;test improper-template-list parsing
(let* ([template 
        (parse-transformer-template
         '(a ... ... #f "c" #\5 6 |.| ((very nested) |.| lists))
         (set 'a))])
  (check-equal?
   template
   (improper-template-list 
    '(a ... ... #f "c" #\5 6 |.| ((very nested) |.| lists))
    (list
     (ellipses-template
      'a ;TODO syntax should probably include the ellipses too
      (template-identifier 'a)
      2
      (set 'a))
     (template-datum #f)
     (template-datum "c")
     (template-datum #\5)
     (template-datum 6))
    (improper-template-list 
     '((very nested) |.| lists)
     (list
      (template-list
       '(very nested)
       (list
        (template-identifier 'very)
        (template-identifier 'nested))))
     (template-identifier 'lists)))))

;test combination of an ellipses matcher and template to match syntax and then output it
;tests when an identifier appears in multiple positions in a template with different ellipses nestings
;tests when the ellipses nesting between matcher and template don't match
(let* ([pattern
        (parse-transformer-pattern
         '((a ...) b) (set))]
       [pattern-nesting 
        (compute-ellipses-nesting pattern)]
       [pattern-ids (apply set (hash-keys pattern-nesting))]
       [template1
        (parse-transformer-template
         '(a ...) pattern-ids)]
       [template2
        (parse-transformer-template
         '(b c d f) pattern-ids)]
       [template3 
        (parse-transformer-template
         '(g (f 'b (h a ...) "a" b)) pattern-ids)]
       [bad-template1
        (parse-transformer-template
         '((a b) ...) pattern-ids)]
       [bad-template2
        (parse-transformer-template
        '(a (a ...) b) pattern-ids)])
  (for ([template (list template1 template2 template3)])
    (verify-template-ellipses-nesting template pattern-nesting))
  (for ([template (list bad-template1 bad-template2)])
    (check-exn syntax-error? 
               (lambda () 
                 (verify-template-ellipses-nesting template pattern-nesting)))))

;is this two tests in one?
;the first test checks that an error is thrown if there is no identifier in an ellipses template
;the second, tests that we can find an identifier deep inside of an ellipses list
(let* ([bad-syntax
        '(5 ...)]
       [really-nested-identifier-syntax
        '((1 2 3 |.| (4 5 |.| ('c "d" a #\g))) ...)])
  (check-exn syntax-error? (lambda () (parse-transformer-template bad-syntax (set 'a))))
  (parse-transformer-template really-nested-identifier-syntax (set 'a))
  #t)


;tests that verify-template-ellipses-nesting works correctly with 
; varying ways of nesting an identifier in multiple ellipses in a template
(let* ([pattern
         (parse-transformer-pattern
          '((((a ...) ...)) ...) (set))]
        [nesting (compute-ellipses-nesting pattern)]
        [good1
          (parse-transformer-template
           '((a ... ...) ...)
           (set 'a))]
        [good2
         (parse-transformer-template
          '(a ... ... ...)
          (set 'a))]
        [good3
         (parse-transformer-template
          '((a ...) ... ...)
          (set 'a))]
        [bad1
         (parse-transformer-template
          '((a ... ... ...) ...)
          (set 'a))]
        [bad2
         (parse-transformer-template
          '((a ... ...) ... ...)
          (set 'a))])
   (for ([t (list good1 good2 good3)])
     (verify-template-ellipses-nesting t nesting))
   (for ([t (list bad1 bad2)])
     (check-exn syntax-error? 
                (lambda () 
                  (verify-template-ellipses-nesting t nesting)))))

;test flatten#
(let* ([3-level 
        '(
          ((a b c) (d e f)) 
          ((g h i) (j k l)) 
          ((m n o) (p q r) (s t u)))])
  (check-equal?
   (flatten# 3-level 1)
   '((a b c) (d e f) (g h i) (j k l) (m n o) (p q r) (s t u)))
  (check-equal?
   (flatten# 3-level 2)
   '(a b c d e f g h i j k l m n o p q r s t u)))

;more complex flatten# test
(let ([4-level
        '(
          (
           (
            (a b) (c d))
           (
            (e f) (g h))
           (
            (i j) (k l) (m n))
            (
             (o p q r)))
          (
           (
            (s t))
           (
            (u v) (w x) (y z 1 2 3))))])
  (check-equal? (flatten# 4-level 0) 4-level)
  (check-equal?
   (flatten# 4-level 1)
   '(
      (
       (a b) (c d))
      (
       (e f) (g h))
      (
       (i j) (k l) (m n))
      (
       (o p q r))
      (
       (s t))
      (
       (u v) (w x) (y z 1 2 3))))
  (check-equal?
   (flatten# 4-level 2)
   '(
     (a b) (c d) (e f) (g h)
           (i j) (k l) (m n) 
           (o p q r) (s t) (u v) 
           (w x) (y z 1 2 3)))
  (check-equal? 
   (flatten# 4-level 3)
   '(a b c d e f g h i j k l m n o p q r s t u v w x y z 1 2 3))
  (check-exn exn:fail:contract?
               (lambda () (flatten# 4-level 4))))
                  
;tests find-regular-ids, make-rewriter. 
;Basically a template is created and used to make a macro.
;That macro is then invoked and its resulting syntax is checked.
(let* 
    ([pattern-ids (set 'a 'b 'c 'd)]
     [template
      (parse-transformer-template
       '((a ...) ((b c (d ...)) ...) . (e f g ('h "i" (#\k)) 'l))
       pattern-ids)]
     [reg-ids (find-regular-ids template pattern-ids)]
     [rewriter (make-rewriter template)]
     [sub-map 
      (hash 
       'a (list 1 2 3)
       'b (list 4 5 6)
       'c (list 7 8 9)
       'd '((10 11 12) (13 14 15) (16 17 18))
       'e 'e.1
       'f 'f.1
       'g 'g.1
       'quote 'quote.1
       'h 'h.1
       'l 'l.1)])
  (check-equal? 
   reg-ids
   (set 'e 'f 'g 'quote 'h 'l))
  (check-equal?
   (rewriter sub-map)
   '((1 2 3) ((4 7 (10 11 12)) (5 8 (13 14 15)) (6 9 (16 17 18))) . 
             (e.1 f.1 g.1 ((quote.1 h.1) "i" (#\k)) (quote.1 l.1)))))

;Helper function to check that an identifier was generated from some base identifier
(define (string-prefix? string prefix)
  (define end (string-length prefix))
  (if (< (string-length string) end)
      #f
      (let loop ([idx 0])
        (cond 
          [(>= idx end) #t]
          [(not (eqv? (string-ref string idx) (string-ref prefix idx))) #f]
          [else (loop (add1 idx))]))))

;curries the second argument of string-prefix? with (symbol->string prefix-sym)
(define (sym-matcher prefix-sym)
  (define prefix (symbol->string prefix-sym))
  (lambda (sym)
    (string-prefix? (symbol->string sym) prefix)))

;simple test of the 'and' macro, which showcases the fact that I don't yet have the ability to parse recursive macros
(let*-values 
    ([(rules) (parse-syntax-rules 
               '(syntax-rules ()
                  ((and) #t)
                  ((and test) test)
                  ((and test1 test2 ...)
                   (if test1 (and test2 ...) #f))) (hash))]
     
     [(macro) (make-macro-transformer rules)]
     [(env) (hash 'and 'and 'if 'if)]
     [(r1 e1) (macro '(and) env)]
     [(r2 e2) (macro '(and "bob") env)]
     [(r3 e3) (macro '(and #f #t) env)])
  (check-equal? #t r1)
  (check-equal? "bob" r2)
  (check-not-exn
   (lambda ()
     (match r3
       [(list (? (sym-matcher 'if)) #f (list (? (sym-matcher 'and)) #t) #f)
        #t]))))

;test to show that we are handling quote forms properly by not treating them specially, since we don't know the exact binding of each quote identifier yet
(let*-values
    ([(rules) (parse-syntax-rules
               '(syntax-rules ()
                  [(who-cares a) (list a 'a)]) (hash))]
     [(macro) (make-macro-transformer rules)]
     [(env) (hash)]
     [(r1 e1) (macro '(imagine-this-keyword-is-bound-to-the-macro (list 1 2 3)) (hash))])
  (check-not-exn
   (lambda ()
     (match r1
     [(list (? (sym-matcher 'list)) (list 'list 1 2 3) (list (? (sym-matcher 'quote)) (list 'list 1 2 3))) #t]))))