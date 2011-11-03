#lang racket

(require racket rackunit
         "clinger-rees-syntax-rules.rkt"
         "clinger-rees-parser.rkt"
         "clinger-rees-env.rkt")

;All of these examples were run through Racket's runtime. The results were used to create a test suite to verify
; that this expander behaves the same as Racket's.
(define program '(
(define-syntax dual-nesting
  (syntax-rules ()
    [(_ (l1 ...) (l2 ...) ...)
     (list '(l1 l2) ... ...)]))

(define-syntax dual-nesting-1-3
  (syntax-rules ()
    [(_ (l1 ...) ((l3 ...) ...) ...) (list '(l1 l3) ... ... ...)]))

(define-syntax dual-nesting-2-3
  (syntax-rules ()
    [(_ ((l2 ...) ...) (((l3 ...) ...) ...)) (list '(l2 l3) ... ... ...)]))

(define-syntax varied-nesting
  (syntax-rules ()
    [(_ (l1 ...) ((l2 ...) ...) (((l3 ...) ...) ...))
     (list (list (list (list l1 l2) ...) (list (list l1 l3) ... ...)) ...)]))

(define-syntax varied-nesting2
  (syntax-rules ()
    [(_ (l1 ...) ((l2 ...) ...) (((l3 ...) ...) ...))
     (list (list (list (list l1 l2) ...) (list (list l2 l3) ... ...)) ...)])) ;l2 gets flattened to a different level for different sub-expressions of the same ellipses templatee

(define-syntax varied-nesting2.2
  (syntax-rules ()
    [(_ (l1 ...) ((l2 ...) ...) (((l3 ...) ...) ...))
     (list (list (list (list l1 l2) ...) (list (list l2 l3) ... ... ...)) ...)])) 

  ;A macro that showcases how inner ellipses templatees can operate entirely independently of their outer ellipses.
))

(define (expand-expr syntax)
  (define-values (te expanded) (expand-program top-env (list syntax)))
  (set! top-env te)
  expanded)

(define ns (make-base-namespace))

(define-syntax test
  (syntax-rules ()
    [(_ syntax expected)
     (begin
       (define expanded (expand-expr (quote syntax)))
       (display expanded) (display "\n")
       (check-equal? (eval `(begin ,@expanded) ns) (quote expected)))]))

(define-syntax test-syntax-error
  (syntax-rules ()
    [(_ syntax)
     (check-exn syntax-error? (λ () (expand-expr (quote syntax))))]))

(define-values (top-env expanded) (expand-program r5rs-top-level-env program))
(eval `(begin ,@expanded) ns)

(test
 (dual-nesting ())
 ())


(test
 (dual-nesting (I'm naughty syntax))
 ())

(test
 (dual-nesting (1 2 3) (4 5 6) (7 8 9))
 ((1 4) (2 5) (3 6) (1 7) (2 8) (3 9)))

(test
 (dual-nesting (1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))
 ((1 4) (2 5) (3 6) (1 7) (2 8) (3 9) (1 10) (2 11) (3 12) (1 13) (2 14) (3 15)))


(test-syntax-error
 (dual-nesting (1 2) (4 5 6) (7 8 9)))
 
(test-syntax-error
 (dual-nesting (1 2 3 4) (5 6 7) (8 9 0)))

(test-syntax-error
 (dual-nesting (1 2 3) (4 5 6) (7 8)))

(test-syntax-error
 (dual-nesting (1 2 3) (4 5 6) (7 8 9 0)))


(test
 (dual-nesting-1-3 ()  (()))
 ())

(test
 (dual-nesting-1-3 (1 )  ((()) (())))
 ((1 ()) (1 ())))

(test
 (dual-nesting-1-3 (1 2)  ((() ()) (() ())) ((() ())))
 ((1 ()) (2 ()) (1 ()) (2 ()) (1 ()) (2 ())))

(test
 (dual-nesting-1-3 (1) ((2) (3)) ((4) (5)))
 ((1 2) (1 3) (1 4) (1 5)))

(test
 (dual-nesting-1-3 (1 2) ((3 4) (5 6)) ((7 8) (9 10)) ((11 12) (13 14)))
 ((1 3) (2 4) (1 5) (2 6) (1 7) (2 8) (1 9) (2 10) (1 11) (2 12) (1 13) (2 14)))

(test
 (dual-nesting-1-3 (1) ((2) (3)) ((4) (5) (6) (7) (8)))
 ((1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8))) ;second level nestings to not have to be the same length.

(test-syntax-error 
 (dual-nesting-1-3 ()  ((()))))

(test-syntax-error
 (dual-nesting-1-3 (1 2) ((2) (3)) ((4) (5))))

(test-syntax-error
 (define-syntax dual-nesting#1
   (syntax-rules ()
     [(_ (l1 ...) (l2 ...) ...)
      (list '(l1 l2) ...)])) ;too few ellipses for pattern variable in template in: l2
 )

(test-syntax-error
 (define-syntax dual-nesting#2
   (syntax-rules ()
     [(_ (l1 ...) (l2 ...) ...)
      (list '(l1) ... ...)]))
 )

(test (dual-nesting-2-3 () ()) ())
(test (dual-nesting-2-3 () (())) ())
(test (dual-nesting-2-3 (()) ((()))) ())
(test (dual-nesting-2-3 (() ()) ((() ()))) ())
(test (dual-nesting-2-3 (() ()) ((() ()) (() ()) (() ()))) ())
(test (dual-nesting-2-3 ((() ()) (() ())) ()) ())

(test
 (dual-nesting-2-3 (() (1) (2 3) (4 5 6)) ((() (7) (8 9) (10 11 12)) (() (13) (14 15) (16 17 18))))
 ((1 7) (2 8) (3 9) (4 10) (5 11) (6 12) (1 13) (2 14) (3 15) (4 16) (5 17) (6 18)))

(test
 (dual-nesting-2-3 ((1 2 3) (4 5 6)) (((7 8 9) (10 11 12)) ((13 14 15) (16 17 18))))
 ((1 7) (2 8) (3 9) (4 10) (5 11) (6 12) (1 13) (2 14) (3 15) (4 16) (5 17) (6 18)))

(test
 (dual-nesting-2-3 (((bob dole) (is number one)) ((1 2 3 4 5) (1 2))) ()) ;this shows that the less nested variable doesn't have to be valid
 ())

(test-syntax-error
 (dual-nesting-2-3 ((1 2) (3 4)) (((5 6) (7 8)) ((9 10) (11 12) (13 14)))))

(test-syntax-error
 (dual-nesting-2-3 ((1 2) (3 4) (5 6)) (((5 6) (7 8)) ((9 10) (11 12)))))

(test-syntax-error
 (dual-nesting-2-3 ((1 2) (3 4)) (((5 6) (7 8)) ((9) (11 12)))))


(test-syntax-error
 (dual-nesting-2-3 (() (1) (2 3) (4 5 6)) (((7) () (8 9) (10 11 12)) (() (13) (14 15) (16 17 18)))))

(test
 (varied-nesting (1 2 3) ((4 5 6) (7 8 9)) (((10 11 12) (13 14 15)) ((16 17 18) (19 20 21)))) 
 ((((1 4) (2 5) (3 6)) ((1 10) (2 11) (3 12) (1 13) (2 14) (3 15))) (((1 7) (2 8) (3 9)) ((1 16) (2 17) (3 18) (1 19) (2 20) (3 21)))))

(test
 (varied-nesting2 (1 2 3) ((4 5 6) (7 8 9)) (((10 11 12) (13 14 15)) ((16 17 18) (19 20 21)))) 
 ((((1 4) (2 5) (3 6)) ((4 10) (5 11) (6 12) (7 13) (8 14) (9 15))) (((1 7) (2 8) (3 9)) ((4 16) (5 17) (6 18) (7 19) (8 20) (9 21)))))



(test 
 (varied-nesting2.2 (1 2 3) ((4 5 6) (7 8 9)) (((10 11 12) (13 14 15)) ((16 17 18) (19 20 21))))
 ((((1 4) (2 5) (3 6)) ((4 10) (5 11) (6 12) (7 13) (8 14) (9 15) (4 16) (5 17) (6 18) (7 19) (8 20) (9 21)))
  (((1 7) (2 8) (3 9)) ((4 10) (5 11) (6 12) (7 13) (8 14) (9 15) (4 16) (5 17) (6 18) (7 19) (8 20) (9 21)))))
