#lang racket
(define-syntax dual-nesting
  (syntax-rules ()
    [(_ (l1 ...) (l2 ...) ...)
     (list '(l1 l2) ... ...)]))

#;(define-syntax dual-nesting#1
  (syntax-rules ()
    [(_ (l1 ...) (l2 ...) ...)
     (list '(l1 l2) ...)])) ;too few ellipses for pattern variable in template in: l2

#;(define-syntax dual-nesting#2
  (syntax-rules ()
    [(_ (l1 ...) (l2 ...) ...)
     (list '(l1) ... ...)])) ;too many ellipses in template

(define-syntax dual-nesting-1-3
  (syntax-rules ()
    [(_ (l1 ...) ((l3 ...) ...) ...) (list '(l1 l3) ... ... ...)]))

(define-syntax dual-nesting-2-3
  (syntax-rules ()
    [(_ ((l2 ...) ...) (((l3 ...) ...) ...)) (list '(l2 l3) ... ... ...)]))

(dual-nesting ())
(dual-nesting (I'm naughty syntax))
(dual-nesting (1 2 3) (4 5 6) (7 8 9))
(dual-nesting (1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15))
;syntax: incompatible ellipsis match counts for template in: ...
#;(dual-nesting (1 2) (4 5 6) (7 8 9))
#;(dual-nesting (1 2 3 4) (5 6 7) (8 9 0))
#;(dual-nesting (1 2 3) (4 5 6) (7 8))
#;(dual-nesting (1 2 3) (4 5 6) (7 8 9 0))

(dual-nesting-1-3 ()  (()))
(dual-nesting-1-3 (1 )  ((()) (())))
(dual-nesting-1-3 (1 2)  ((() ()) (() ())) ((() ())))
(dual-nesting-1-3 (1) ((2) (3)) ((4) (5)))
(dual-nesting-1-3 (1 2) ((3 4) (5 6)) ((7 8) (9 10)) ((11 12) (13 14)))
(dual-nesting-1-3 (1) ((2) (3)) ((4) (5) (6) (7) (8))) ;second level nestings to not have to be the same length! this means the more nested variable doesn't have to be a matrix.
;syntax: incompatible ellipsis match counts for template in: ...
#;(dual-nesting-1-3 ()  ((())))
#;(dual-nesting-1-3 (1 2) ((2) (3)) ((4) (5)))

(dual-nesting-2-3 () ())
(dual-nesting-2-3 () (()))
(dual-nesting-2-3 (()) ((())))
(dual-nesting-2-3 (() ()) ((() ())))
(dual-nesting-2-3 (() ()) ((() ()) (() ()) (() ())))
(dual-nesting-2-3 ((() ()) (() ())) ())
(dual-nesting-2-3 (() (1) (2 3) (4 5 6)) ((() (7) (8 9) (10 11 12)) (() (13) (14 15) (16 17 18)))) ;this shows that neither more or less nested variable has to be a matrix
(dual-nesting-2-3 ((1 2 3) (4 5 6)) (((7 8 9) (10 11 12)) ((13 14 15) (16 17 18))))
(dual-nesting-2-3 (((bob dole) (is number one)) ((1 2 3 4 5) (1 2))) ()) ;this shows that the less nested variable doesn't have to be valid
#;(dual-nesting-2-3 ((1 2) (3 4)) (((5 6) (7 8)) ((9 10) (11 12) (13 14))))
#;(dual-nesting-2-3 ((1 2) (3 4) (5 6)) (((5 6) (7 8)) ((9 10) (11 12))))
#;(dual-nesting-2-3 ((1 2) (3 4)) (((5 6) (7 8)) ((9) (11 12))))
#;(dual-nesting-2-3 (() (1) (2 3) (4 5 6)) (((7) () (8 9) (10 11 12)) (() (13) (14 15) (16 17 18))))

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
  ;we can apply in excess beyond what l3 can handle? l2 is the anchor here. 
  ;If it gets deleted from (list l1 l2), or even if the entire (list (list l1 l2) ...) expression gets deleted, the template becomes invalid
  ;that means the template only needs one variable that with a matching pattern nesting.
  ;even then, that variable gets flattened differently in sub-expressions!

(varied-nesting (1 2 3) ((4 5 6) (7 8 9)) (((10 11 12) (13 14 15)) ((16 17 18) (19 20 21)))) ;this shows that nesting can vary for the same pattern with the same results
(varied-nesting2 (1 2 3) ((4 5 6) (7 8 9)) (((10 11 12) (13 14 15)) ((16 17 18) (19 20 21)))) ;this shows that nesting can vary for the same pattern with different results!
(varied-nesting2.2 (1 2 3) ((4 5 6) (7 8 9)) (((10 11 12) (13 14 15)) ((16 17 18) (19 20 21))))

