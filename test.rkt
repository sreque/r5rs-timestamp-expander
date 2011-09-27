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
  (let ([a (lambda (v) (format "produced by lambda: ~a\n" v))])
    (display (a 2))))


(define-syntax r1
  (syntax-rules ()
    [(_ syntax ...) (r2 syntax ...)]))

#;(r1 1 2 3) ;fails because r2 not defined yet.
(define-syntax r2
  (syntax-rules ()
    [(_ syntax ...) (apply + (list syntax ...))]))

(r1 10 20 30)

(define (a . b)   ;this is either undefined or illegal by the r5rs spec, but the Racket interpreter handles it just fine. We will follow racket's example
  (printf "~a\n" b))
(a 1 2 3 4 5)

(define (add . xs) (apply + xs))
;Racket exhibits strange behavior here! With this line, the interpreter no longer treats + as a special or pre-defined form for the ENTIRE file!
;This means that code before this point now fails, because it uses + before the definition below, even though you'd
; expect + to have its original definition until the re-binding that occurs below, which, at least, according to r5rs, has the meaning of set! if the symbol had a binding already.
#;(define (+ . xs) (map (λ (v) (add1 v)) xs)) 
(add 1 2 3)

(define-syntax plus-macro
  (syntax-rules ()
    [(_) +]))

(define-syntax plus-macro2
  (syntax-rules ()
    [(_ args ...) (+ args ...)]))

(define-syntax plus-plus-macro
  (syntax-rules ()
    [(_) plus-macro]))

;illegal. you can't reduce to a macro and then apply it as part of an outer expression
#;((plus-plus-macro) 1 2 3 4)

(define-syntax quote-macro
  (syntax-rules ()
    [(_) quote]))

;illegal. you can't reduce to a quote keyword and apply it as part of an outer expression
#;((quote-macro) these symbols are not defined)

;how to make new identifiers painted the same as another
(define-syntax (define-id stx)
  (define subs (syntax->list stx))
  (define id (cadr subs))
  (define value (caddr subs))
  (datum->syntax
   stx
   `(define 
      ,(string->symbol (format "~a-suffix" (symbol->string (syntax->datum id))))
      ,value)))

(define-id bob 2)
(+ bob-suffix 98)