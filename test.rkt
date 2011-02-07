#lang scheme
(require "timestamp-expander.rkt")

(printf "~a\n" (ts-syntax 'a 0))
(define stx1 (quote (a 1 a "b")))

(printf "~a\n" (timestamp-syntax stx1 0))

(define-syntax flatten
  (syntax-rules ()
    ((flatten (v ...) ...) ((flatten v) ... ...))
    #;((flatten (v ...)) ((flatten v) ...))
    ((flatten v) v)))

#;(list (flatten (1 2 3) (4 (5) 6) ((7 8 (9 10 11) 12 (13 (14 15 16))))))

(define-syntax triples
  (syntax-rules (-)
    [(_ ((((a ...) (b ...) (c ...)) ...) -) ...)
     #;(list (list (list a ... b ... c ...) ...) ...)
     #;(list (list a ... ... b ... ... c ... ...) ...)
     (list (list a ... b ... c ...) ... ...)
     #;(list a ... ... ...)
     ]))

(triples ((((1 2 3) (4 5 6) (7 8 9)) 
           ((10 11 12) (13 14 15) (16 17 18)))
          -)
         ((((19 20 21) (22 23 24) (25 26 27)) 
           ((28 29 30) (31 32 33) (34 35 36)))
          -))

(define-syntax improper-test
  (syntax-rules ()
    [(_ a b c . rest)
     (begin
       (printf "first 3: ~a ~a ~a\n" a b c)
       (printf "rest: ~a\n" (quote rest)))]))

(improper-test 1 2 3)

#;(define-syntax dup-pattern-test
  (syntax-rules ()
    [(_ x x) "values in the pair are the same"]
    [(_ x y) "values in the pair are different"]))
#;(dup-pattern-test 'a 'a)
#;(dup-pattern-test 'a 'b)
    