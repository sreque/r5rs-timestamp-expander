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

(define-syntax bad-ellipses
  (syntax-rules ()
    #;[(_ a ignored ... b) (list a b)]
    [(_ ...) 'hi]))

(bad-ellipses 1 2 3 4 5 6)

(define-syntax ellipses-no-patterns
  (syntax-rules ()
    [(_ a ...) '((a 6) ...)]
    [(_ 5 ...) 'hi]))

(ellipses-no-patterns)


(let ([a 1])
  (define-syntax literal-template
  (syntax-rules (a)
    [(_ a) a]))
  (literal-template a))

#;(define-syntax dup-ids
  (syntax-rules ()
    [(_ a a) a]))

#;(dup-ids 1 1)

(define-syntax test-no-rules
  (syntax-rules ()))
#;(test-no-rules)

(define b "b")
(define-syntax quote-test
  (syntax-rules ()
    [(_ a)
     (list a 'a 'b b)]))
(quote-test (list 1 2 3))

;syntax error
#;(define empty-lambda
  (lambda ()))
#;(empty-lambda)
(define dot-lambda
  (lambda (|.| . rest)
    (+ |.| (apply + rest))))
(dot-lambda 1 2 3)

(let-syntax ([a 
              (syntax-rules () 
                [(a v) (format "produced by macro: ~a\n" v)])])
  (display (a 1))
  (let ([a (lambda (v) (format "produced by lambda: ~a" v))])
    (display (a 2))))