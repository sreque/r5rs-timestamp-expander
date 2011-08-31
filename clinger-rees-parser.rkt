(module clinger-rees-parser racket
  (provide (all-defined-out))
  (require racket
           "clinger-rees-syntax-rules.rkt")
  
  ;(define (syntax-definition? form)
    
  (define (verify-binding-form-shape binding-form)
    (when (not (list? binding-form))
        (raise-syntax-error#
         binding-form
         "binding form must be a list"))
    (when (not (eqv? 2 (length binding-form)))
      (raise-syntax-error#
       binding-form
       "binding form must be a list of length 2"))
    (when (not (symbol? (car binding-form)))
      (raise-syntax-error#
       (car binding-form)
       "first value of binding form must be an identifier"))
    ;parse-syntax-rules already throws exceptions with human-readable error messages. 
    ;I should factor that code out into a separate verify method
    #;(when not (syntax-definition? (cadr binding-form))        
      (raise-syntax-error#
       (cadr binding-form)
       "second value of binding form must be a syntax definition")))
  
  (define (verify-syntax-binding-shape syntax form-name)
    (when (not (eqv? 2 (length syntax)))
        (raise-syntax-error# 
         syntax
         (format "expected exactly 2 arguments to ~a, got ~a" (length syntax) form-name)))
    (when (not (list? (car syntax)))
      (raise-syntax-error#
       (car syntax)
       (format "first argument to ~a must be a list" form-name)))
    (for ([binding-form (car syntax)])
      (verify-binding-form-shape binding-form)))
    
  (define (reduce-let-syntax syntax def-env)
    (verify-syntax-binding-shape syntax "let-syntax")
    (let* ([extended-env 
            (for/fold ([result def-env])
              ((binding-form (car syntax)))
              (hash-set result 
                        (car binding-form)
                        (parse-syntax-transformer (cadr binding-form) def-env)))])
      (values (cadr syntax) extended-env)))
 
  (define (reduce-letrec-syntax syntax def-env)
    (define (make-delayed-transformer ref)
      (lambda (syntax use-env orig-sym-env)
        ((unbox ref) syntax use-env orig-sym-env)))
    (define (make-premature-use-macro symbol)
      (lambda (syntax use-env orig-sym-env)
      (raise-syntax-error#
       syntax
       "Cannot recursively invoke a macro until its binding is complete")))
    (verify-syntax-binding-shape syntax "letrec-syntax")
    (let* ([binding-forms (car syntax)]
           [ref-map
            (for/hash ((binding-form binding-forms))
              (values (car binding-form) (box (make-premature-use-macro (car binding-form)))))]
           [extended-env 
            (for/fold ([result def-env])
              ((binding-form binding-forms))
              (define id (car binding-form))
              (define ref (hash-ref ref-map id))
              (define delayed-macro (make-delayed-transformer ref))
              (hash-set result id delayed-macro))])
      (for ([binding-form binding-forms])
        (define ref (hash-ref ref-map (car binding-form)))
        (define transformer (parse-syntax-transformer (cadr binding-form) extended-env))
        (set-box! ref transformer))
      (values (cadr syntax) extended-env)))
         
     
)