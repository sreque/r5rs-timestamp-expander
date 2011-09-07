(module clinger-rees-parser racket
  (provide (all-defined-out))
  (require racket
           "clinger-rees-syntax-rules.rkt")
  ;our goal in this file is to define methods that can handle all the primitive forms that aren't easily defined in terms of another form
  ;  top-level define, local define, define-syntax, let-syntax, letrec-syntax, lambda
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
  
  (define (verify-lambda-formals formals)
      (define (add-id s v)
        (if (symbol? v)
            (if (set-member? s v)
                (raise-syntax-error#
                 formals
                 (format "Duplicate identifier detected in argument list: ~a" v))
                (set-add s v))
            (raise-syntax-error#
             v
             (format "invalid argument to a formals list: `~a'. All members should be identifiers" v))))
      (let loop ([rem formals]
                 [ids (set)])
        (if  (or (cons? rem) (null? rem))
             (if (empty? rem)
                 ids
                 (loop (cdr rem) (add-id ids (car rem))))
             (add-id ids rem))))
  
  ;verifies the shape of the lambda form and returns the set of identifiers the form binds
  ; if this function ever became part of something other than an exander, it could be modified
  ; to return information about the formals 
  (define (verify-lambda-shape syntax)
    (when (< (length syntax) 2)
      (raise-syntax-error#
       syntax
       "lambda form requires at least an argument list and one expression"))
    (verify-lambda-formals (car syntax)))
  
  (define (reduce-lambda syntax env)
    (define ids (verify-lambda-shape syntax))
    (define (extend-env)
      (for/fold ([result env])
        ((id ids))
        (hash-set result id id)))
    (values (cdr syntax) (extend-env)))
  
  ;verifies that a define is shaped correctly and then returns the identifier the define binds
  (define (verify-define-shape syntax)
    (cond
      [(< (length syntax) 2)
       (raise-syntax-error#
        syntax
        "define form does not have the required arguments")]
      [(symbol? (car syntax))
       (if (or (null? (cdr syntax)) (not (null? (cddr syntax))))
           (raise-syntax-error#
            syntax
            "define form with one variable as its first argument must have exactly one expression following the identifier")
           (car syntax))]
      [(cons? (car syntax))
       (if (null? (car syntax))
           (raise-syntax-error#
            syntax
            "The first argument to a define form, if a list, must contain at least one identifier")
           (caar syntax))]
      [else (raise-syntax-error# 
             syntax
             "First argument to a define form must be an identifier or a list of an identifier prepended to a formals list")]))
  
  (define (reduce-define syntax env)
    (define id (verify-define-shape syntax))
    (define body 
      (if (symbol? (car syntax))
          (cadr syntax)
          `(lambda ,(cdar syntax) ,@(cdr syntax))))
    (values (hash-set env id id) body))
                   
           
)