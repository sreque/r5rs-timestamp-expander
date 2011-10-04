#lang racket
(require racket rackunit
         "clinger-rees-syntax-rules.rkt"
         "clinger-rees-parser.rkt"
         "clinger-rees-env.rkt")

;copy-pasted from clinger-rees-syntax-rules-test.rkt
(define (string-prefix? string prefix)
  (define end (string-length prefix))
  (if (< (string-length string) end)
      #f
      (let loop ([idx 0])
        (cond 
          [(>= idx end) #t]
          [(not (eqv? (string-ref string idx) (string-ref prefix idx))) #f]
          [else (loop (add1 idx))]))))

;copy-pasted from clinger-rees-syntax-rules-test.rkt
(define (sym-matcher prefix-sym)
  (define prefix (symbol->string prefix-sym))
  (lambda (sym)
    (string-prefix? (symbol->string sym) prefix)))
(define-syntax check-syntax-error
  (syntax-rules ()
    [(_ action ...)
     (check-exn syntax-error? (lambda () action ...))]))

;test that let-syntax returns the expected environment and body syntax
;TODO test that the parsed macros actually work properly
(let*-values
    ([(body-syntax)
      '(when #t
         (unless #f
           5))]
     [(body-syntax-returned extended-env)
      (reduce-let-syntax
       `(((when (syntax-rules ()
              [(when test stmt1 stmt2 ...) 
               (if test
                   (begin stmt1 stmt2 ...) (void))]))
         (unless (syntax-rules ()
                   [(unless test stmt1 stmt2 ...)
                    (if (not test)
                        (begin stmt1 stmt2 ...) (void))])))
       ,body-syntax) 
       (hash))])
  (check-equal? (set 'when 'unless) (apply set (hash-keys extended-env)))
  (check-equal? `(,body-syntax) body-syntax-returned))

;test definition of 'and' macro with letrec
(let*-values
    ([(and-macro-syntax)
      '(syntax-rules ()
         ((my-and) #t)
         ((my-and test) test)
         ((my-and test1 test2 ...)
          (if test1 (my-and test2 ...) #f)))]
     [(body-syntax)
      '(my-and 1 2 3 4)]
     [(body-syntax-returned env)
      (reduce-letrec-syntax
       `(((my-and ,and-macro-syntax))
         ,body-syntax) (hash 'if 'if))]
     [(macro) (hash-ref env 'my-and)]
     [(s1 e1 qe1) (macro body-syntax-returned env (hash))])
  (check-equal? (set 'my-and 'if) (apply set (hash-keys env)))
  ;TODO convert these print statements into actual assertions. 
  ;The print statements look like they are printing out the right things for now.
  (printf "~a\n" s1)
  (printf "~a\n" e1))

;test verify lambda formals
(begin
  (check-equal? (verify-lambda-formals '(a b c d e f g)) (apply set '(a b c d e f g)))
  (check-equal? (verify-lambda-formals '(a . b)) (set 'a 'b))
  (check-equal? (verify-lambda-formals '(|.| . dot)) (set '|.| 'dot))
  (check-equal? (verify-lambda-formals '()) (set))
  (check-equal? (verify-lambda-formals 'variable-arity) (set 'variable-arity))
  (check-syntax-error (verify-lambda-formals '(())))
  (check-syntax-error (verify-lambda-formals '(a b . #(my vector))))
  (check-syntax-error (verify-lambda-formals '(id id2 id3 number5 5 . a)))
  )

;test verify lambda shape
(begin
  (check-syntax-error (verify-lambda-shape '()))
  (check-syntax-error (verify-lambda-shape '(())))
  (check-equal? (verify-lambda-shape '(() 5)) (set))
  (check-equal? (verify-lambda-shape '((a b . c) 1 2 (3 (4 (5))))) (set 'a 'b 'c))
  )

;test reduce-lambda
(let*-values
    ([(formals) '(a b c )]
     [(body) '(+ a b c)]
     [(lambda-expr) `(,formals ,body)]
     [(_body extended-env) (reduce-lambda lambda-expr (hash))])
  (check-equal? _body (list body))
  (check-not-exn
   (λ ()
     (match extended-env
       [(hash-table ('a (? (sym-matcher 'a))) ('b (? (sym-matcher 'b))) ('c (? (sym-matcher 'c)))) #t]))) )

;test reduce-lambda with multiple body expressions
(let*-values
    ([(expr1) '(+ 1 2 3)]
     [(expr2) '#(a b c)]
     [(expr3) '#{1 2 3 4}]
     [(body env) (reduce-lambda `(() ,expr1 ,expr2 ,expr3) (hash))])
  (check-equal? env (hash))
  (check-equal? body (list expr1 expr2 expr3)))

;test verify-define
(begin
  (check-syntax-error (verify-define-shape '()))
  (check-syntax-error (verify-define-shape '(a)))
  (check-syntax-error (verify-define-shape '(())))
  (check-syntax-error (verify-define-shape '(a b c)))
  (check-equal? (verify-define-shape '(a #(vector bob))) 'a)
  (check-equal? (verify-define-shape '((a . b) body)) 'a) ;not in r5rs spec, but Racket supports it
  (check-equal? (verify-define-shape '((a b c d e f g . h) body)) 'a))

;test reduce define
(let*-values
    ([(var-def) '(define a (if #t true false))]
     [(var-lambda-def) '(define (a b c) + 1 2 3 4 5)]
     [(var-env var-body) (reduce-define (cdr var-def) (hash))]
     [(var-lambda-env var-lambda-body) (reduce-define (cdr var-lambda-def) (hash))])
  (check-equal? var-env (hash 'a 'a))
  (check-equal? var-lambda-env (hash 'a 'a))
  (check-equal? var-body (caddr var-def))
  (check-equal? var-lambda-body '(lambda (b c) + 1 2 3 4 5))
  (check-syntax-error (reduce-lambda (list #{a b c d}) (hash))))
     
;test unquote
(let*-values
    ([(quote-expr) '(quote (a b c 1 2 3 "you" "and" "me" we-make-three))]
     [(macro) (parse-syntax-transformer
               `(syntax-rules ()
                  [(id) ,quote-expr])
               (hash))]
     [(syntax local-env quote-env) (macro '(id) (hash) (hash))])
  (check-not-exn
   (λ () 
     (match syntax
       [(list (? (sym-matcher 'quote)) (list (? (sym-matcher 'a)) (? (sym-matcher 'b)) (? (sym-matcher 'c)) 1 2 3 "you" "and" "me" (? (sym-matcher 'we-make-three)))) #t])))
  (check-equal? (unquote-syntax syntax quote-env) quote-expr)
  (check-equal? (expand-inner-syntax syntax r5rs-top-level-env local-env quote-env) quote-expr))

;test symbol expansion, both user symbols and macro-generated symbols
(let*-values
    ([(sym) 'symbol]
     [(empty-env) (hash)]
     [(sym-defined-env) (hash sym sym)]
     [(macro) (parse-syntax-transformer
               '(syntax-rules ()
                  [(id) (display "hello world!")]) empty-env)]
     [(sym-defined-as-macro-env) (hash sym macro)]
     [(uses-sym-syntax) `(syntax-rules ()
                            [(id)  (,sym 1 2 3)])]
     [(macro-undefined) (parse-syntax-transformer uses-sym-syntax empty-env)]
     [(macro-defined) (parse-syntax-transformer uses-sym-syntax sym-defined-env)])
  (check-equal? (expand-inner-syntax sym empty-env sym-defined-env empty-env) sym)
  (check-equal? (expand-inner-syntax sym sym-defined-env empty-env empty-env) sym)
  (check-exn syntax-error?
             (λ () (expand-inner-syntax sym empty-env empty-env empty-env)))
  (check-exn syntax-error?
             (λ () (expand-inner-syntax sym empty-env (hash-set sym-defined-env sym macro) empty-env)))
  (check-equal?
   (expand-inner-syntax '(id) empty-env (hash-set sym-defined-env 'id macro-defined) empty-env) `(,sym 1 2 3))
  (check-exn syntax-error?
             (λ () (expand-inner-syntax '(id) empty-env (hash-set sym-defined-env 'id macro-undefined) empty-env))))
     
;test expanding data
(let*
    ([expand (λ (v) (expand-inner-syntax v r5rs-top-level-env (hash) (hash)))]
     [test (λ (v) (check-equal? (expand v) v))])
  (test #\d)
  (test "string")
  (test 5.5))

;test expanding literals supported by the Racket reader but not by r5rs
(check-exn syntax-error? (λ () (expand-inner-syntax #&17 r5rs-top-level-env (hash) (hash))))

;vectors not yet supported
(check-exn syntax-error? (λ () (expand-inner-syntax #(1 2 3) (hash) (hash) (hash))))

;an empty list is illegal syntax
(check-exn syntax-error? (λ () (expand-inner-syntax '() (hash) (hash) (hash))))
;TODO: test handle-list

;check procedure application of a symbol
(let*
    ([syntax '(+ 1 2 3)]
     [expanded (expand-inner-syntax syntax r5rs-top-level-env (hash) (hash))])
  (check-not-exn
   (λ () (match expanded [(list (? (sym-matcher '+)) 1 2 3) #t]))))

;check procedure application where the operator is a a list
(let*
    ([syntax '((plus) 4 5 6)]
     [expanded (expand-inner-syntax syntax (hash) (hash 'plus 'plus) (hash))])
  (check-equal? expanded syntax))

;check handling of begin
;NYI requires expand-inner-body-syntax
#;(let*
    ([syntax '(begin (side-effects!) (return-value 'sym))]
     [expanded (expand-inner-syntax syntax
                                    (hash 'side-effects! 'side-effects!)
                                    (hash 'return-value 'return-value)
                                    (hash))])
  (check-equal? expanded '((lambda () (side-effects!) (return-value 'sym)))))

;test macro use
(let*
    
    ([macro-syntax '(syntax-rules () [(macro args ...) (list args ...)])]
     [macro (parse-syntax-transformer macro-syntax (hash))]
     [syntax '(append (macro 1 2 3) (macro 4 5 6))]
     [expanded (expand-inner-syntax syntax (hash 'append 'append 'macro macro 'list 'list) (hash) (hash))])
  (check-not-exn
   (λ () 
     (match expanded
       [(list 'append (list 'list 1 2 3) (list 'list 4 5 6)) #t]))))
     
;test illegal define
(check-exn syntax-error?
           (λ () (expand-inner-syntax '(define a 5) (hash) (hash) (hash))))

;test illegal define-syntax`
(check-exn syntax-error?
           (λ () (expand-inner-syntax '(define-syntax bob (syntax-rules () [(bob dole) '()])) (hash) (hash) (hash))))

;shouldn't be able to apply values that can't resolve to functions
(check-exn
 syntax-error?
 (λ ()
   (expand-inner-syntax '("I'm a function! Really!" 'no 'you 'are 'not) (hash) (hash) (hash))))

;test simple lambda expression expansion
(let ([syntax 
       (expand-inner-syntax 
        '(lambda (a b c) (+ a b c)) (hash '+ '+) (hash) (hash))])
  (check-not-exn
   (λ ()
     (match syntax
       [(list 'lambda 
              (list (? (sym-matcher 'a)) (? (sym-matcher 'b)) (? (sym-matcher 'c))) 
              (list '+ (? (sym-matcher 'a)) (? (sym-matcher 'b)) (? (sym-matcher 'c)))) #t]))))

;simple test case for hygienic macro expansion using let-syntax, letrec-syntax, and lambda
(for ([local-syntax-type (list 'let-syntax 'letrec-syntax)])
  (let*
      ([syntax `((lambda (x)
                   (,local-syntax-type
                       ((m (syntax-rules ()
                             [(m) x])))
                     ((lambda (x) (m)) "inner"))) "outer")]
       [expanded-syntax (expand-inner-syntax syntax (hash) (hash) (hash))])
    (match-define
      (list (list 'lambda (list x1) (list 'begin (list (list 'lambda (list x2) x1) "inner"))) "outer") expanded-syntax)
    (check-not-equal? x1 x2)
    (check-true ((sym-matcher 'x) x1))
    (check-true ((sym-matcher 'x) x2))))

;testing a body syntax list with multiple defines of various shapes
(let*
  ([body-syntax
    '(
      (define a 1)
      (define b 2)
      (define c (lambda () (+ a b)))
      (define (d a b) (+ a b))
      (d (c) (c)))]
   [expanded-syntax
    (cons 'begin (expand-inner-body-syntax body-syntax r5rs-top-level-env (hash) (hash)))])
  ;the resulting syntax is getting complex enough that pattern matching feels like too much work
  (check-equal? 6 (eval expanded-syntax (make-base-namespace))))

;Test macro that expand to begins that contain defines. They should get spliced in.
;also tests macro that expands to a regular define 
;also test syntax that is itself a begin containing definitions
(let*
    ([syntax
      '(let-syntax
           ([definer (syntax-rules ()
                       [(_ a b c) 
                        (begin
                          (define a 1)
                          (define b 2)
                          (define c 3))])]
            [simple-definer (syntax-rules ()
                              [(_ k v)
                               (define k v)])])
         (definer v1 v2 v3)
         (definer more vars here)
         (begin
           (define plus +)
           (define minus -))
         (simple-definer hundred 100)
         (define two 2)
         (plus (minus here vars more) (minus v1 v2 v3) hundred two))]
     [expanded1 
      (expand-inner-syntax syntax r5rs-top-level-env (hash) (hash))]
     [expanded2
      (cons 'begin (expand-inner-body-syntax (list syntax) r5rs-top-level-env (hash) (hash)))])
  (check-equal? 98 (eval expanded1 (make-base-namespace)))
  (check-equal? 98 (eval expanded2 (make-base-namespace))))

(let*
    ([syntax
      '(let-syntax
           ([macro (syntax-rules ()
                     [(macro k v) (begin (define k v) (display v))])])
         (macro a 1)
         (define b 2)
         (+ a b))])
  ;expected illegal define. 
  ;TODO: come up with a way to check the content of the exception
  (check-exn 
   syntax-error?
   (λ () (expand-inner-syntax syntax r5rs-top-level-env (hash) (hash)))))

;simple test of expand-top-level with defines
(let*
    ([syntax
      '(define b 5)]
     [more-syntax
      `(,syntax (* b b))]
     [bad-syntax
      `(,syntax (* b c))])
  (define-values (env expanded) (expand-top-level-form (hash) syntax))
  (define expanded-list (expand-program r5rs-top-level-env more-syntax))
  (check-equal? env (hash 'b 'b))
  (check-equal? syntax expanded)
  (check-equal? expanded-list more-syntax)
  (check-exn syntax-error? (λ () (expand-program r5rs-top-level-env bad-syntax))))

;simple test of top-level define-syntax
(let*
    ([program
      '((define-syntax define-thunk
          (syntax-rules ()
            [(_ key val) (define key (lambda () val))]))
        (define-thunk a 1)
        (define-thunk b 2)
        (display (+ (a) (b)))
        (define another-value 10)
        (* another-value (a) (b)))]
     [expanded-syntax (expand-program r5rs-top-level-env program)])
  (check-equal? 20 (eval `(begin ,@expanded-syntax) (make-base-namespace))))


