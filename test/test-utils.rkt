(module test-utils racket
  (provide (all-defined-out))
  (require racket 
           rackunit
           "../clinger-rees-syntax-rules.rkt"
           "../clinger-rees-parser.rkt"
           "../clinger-rees-env.rkt")
  
  (define-syntax (make-expand-test-defs stx)
    (syntax-case stx ()
      [(_)
       (let-syntax ([with-ids
         (syntax-rules ()
           [(_ (id ...) body ...) 
            (with-syntax ([id (datum->syntax stx 'id)] ...) body ...)])])
         (with-ids 
          ([ns top-env expand-expr expand-expr-list eval-expr eval-expr-list expand-and-eval test-expand test-expand-and-eval test-expand-shape test-syntax-error primitive-eval])
          #'(begin
              (define (expand-expr-generic value handler)
                (define-values (te expanded) (handler top-env value))
                (set! top-env te)
                expanded)
              (define expand-expr (curryr expand-expr-generic expand-top-level-form))
              (define expand-expr-list (curryr expand-expr-generic expand-program))
              (define ns (make-base-namespace))
              (define top-env r5rs-top-level-env)
              (define (eval-expr expr) (eval expr ns))
              (define (eval-expr-list expr-list)
                (eval `(begin ,@expr-list) ns))
              (define (expand-and-eval syntax)
                (let ([expanded (expand-expr-list syntax)])
                  ;(display expanded) (display "\n")
                  (eval-expr-list expanded)))
              (define (datafy data)
                (cond
                  [(or (cons? data) (null? data))
                   `(quote ,(map (λ (v) (datafy v)) data))]
                  [else data]))
              
              (define (primitive-eval data)
                (match data
                  [(list 'quote value) (primitive-eval value)]
                  [(list 'list xs (... ...)) (map (λ (x) (primitive-eval x)) xs)]
                  [(cons a b) (cons (primitive-eval a) (primitive-eval b))]
                  [else data]))
              (define-syntax test-expand-and-eval
                (syntax-rules ()
                  [(_ syntax expected)
                   (begin
                     (check-equal? (eval-expr (expand-expr (quote syntax))) (quote expected)))]))
              
              (define-syntax test-expand
                (syntax-rules ()
                  [(_ syntax expected)
                   (begin
                     (check-equal? (primitive-eval (expand-expr 'syntax)) 'expected))]))
              
              (define-syntax test-expand-shape
                (syntax-rules ()
                  [(_ syntax pattern)
                   (check-not-exn 
                    (λ () 
                      (match (primitive-eval (expand-expr (quote syntax)))
                        [pattern #t])))]))
              
              (define-syntax test-syntax-error
                (syntax-rules ()
                  [(_ syntax)
                   (check-exn syntax-error? (λ () (expand-expr (quote syntax))))])))))]))
  )