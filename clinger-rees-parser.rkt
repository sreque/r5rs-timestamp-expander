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
      (values (cdr syntax) extended-env)))
 
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
      (values (cdr syntax) extended-env)))
  
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
        (hash-set result id (gensym id))))
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
  
  ;returns a symbol if the identifier is bound to a runtime value
  ;returns a procedure if the identifier is bound to a syntax transformer
  ;returns void if the identifier is unbound
  (define (get-denotation symbol top-env local-env)
    (define local-denotation (hash-ref local-env symbol (void)))
    (cond
      [(void? local-denotation) ;unbound locally
       (hash-ref top-env symbol (void))] ;therefore get top level binding
      [(denotation? local-denotation)  ;bound to the top-level denotation of another identifier
       (hash-ref top-env (denotation-id local-denotation) (void))] ;therefore get the top-level denotation of the other identifier
      [else local-denotation])) ;bound locally, therefore return local binding
         
  (define (unquote-syntax syntax quote-env)
    (define rcurried (λ (s) (unquote-syntax s quote-env)))
    (cond
      [(list? syntax)   (map rcurried syntax)]
      [(vector? syntax) (vector-map rcurried syntax)]
      [(symbol? syntax) (hash-ref quote-env syntax syntax)]
      [else             syntax]))
  
  ;expand a piece of syntax that is not at the top level and not at the beginning of a body
  ;this means that define and define-syntax is illegal
  (define (expand-inner-syntax syntax top-env local-env quote-env)
    (define (handle-list)
      (cond
        [(empty? syntax)
         (raise-syntax-error#
          syntax
          "illegal empty list")]
        [(symbol? (car syntax))
         (handle-symbol-application)]
        [else (handle-procedure-application)]))
    (define (handle-procedure-application)
      (map (lambda (child-syntax) (expand-inner-syntax child-syntax top-env local-env quote-env)) syntax))
    (define (handle-local-extension reducer)
      (define-values (body-syntax extended-env) (reducer (cdr syntax) local-env))
      (expand-inner-syntax `(begin @,body-syntax) top-env extended-env))
    (define (handle-symbol-application)
      (define symbol (car syntax))
      (define local-binding (hash-ref local-env symbol (void)))
      (define denotation-symbol
        (if (denotation? local-binding) (denotation-id local-binding) symbol))
      (define denotation (get-denotation symbol top-env local-env))
      (define bound? (not (void? denotation)))
      (define denotes-keyword?
        (if bound?
            (lambda (keyword) #f)
            (lambda (keyword) (eqv? denotation-symbol keyword))))
      #;(printf "symbol=~a denotation-symbol=~a bound?=~a eqv-quote=~a\n" symbol denotation-symbol bound? (denotes-keyword? 'quote))
      (cond
        [(or (denotes-keyword? 'define) (denotes-keyword? 'define-syntax))
         (raise-syntax-error#
          syntax
          (format "~a illegal except at the top level and at the beginning of a body expression" symbol))]
        [(denotes-keyword? 'begin)
         (expand-inner-syntax `((lambda () @,(cdr syntax))) top-env local-env)]
        [(denotes-keyword? 'let-syntax)
         (handle-local-extension reduce-let-syntax)]
        [(denotes-keyword? 'letrec-syntax)
         (handle-local-extension reduce-letrec-syntax)]
        [(denotes-keyword? 'lambda)
         (handle-local-extension reduce-lambda )]
        [(denotes-keyword? 'quote)
         (if (or
              (null? (cdr syntax))
              (not (cons? (cdr syntax)))
              (not (null? (cddr syntax))))
             (raise-syntax-error#
              syntax
              "Quote form must contain exactly one argument")
             (unquote-syntax syntax quote-env))]
        [(procedure? denotation) 
         (define-values (expanded-syntax expanded-local-env expanded-quote-env) 
           (denotation syntax local-env quote-env))
         (expand-inner-syntax expanded-syntax top-env expanded-local-env expanded-quote-env)]
        [else (handle-procedure-application)]))
    (cond
      [(cons? syntax)
       (handle-list)]
      [(vector? syntax)
       (raise-syntax-error#
        syntax
        "support for vector macros not yet added")]
      [(symbol? syntax)
       (define denotation (get-denotation syntax top-env local-env))
       (define user-sym (hash-ref quote-env syntax syntax))
       #;(printf "symbol=~a user-symbol=~a denotation=~a top-env=~a local-env=~a\n" syntax user-sym denotation top-env local-env)
       (cond
         [(procedure? denotation)
           (raise-syntax-error#
            syntax
            (format "Identifier ~a is bound to a macro: which must be applied to arguments" user-sym))]
         [(void? denotation)
          (raise-syntax-error#
           syntax
           (format "Identifier ~a is unbound" user-sym))]
         [(symbol? denotation)
          denotation]
         [else
          (raise-syntax-error#
           syntax
           (format "Unrecognized denotation: ~a.\n This suggests a bug in the macro expander." denotation))])]
      [(syntax-datum? syntax) syntax]
      [else 
       (raise-syntax-error#
        syntax
        "Unrecognized syntax type")]))
           
         
         
          
                  
           
)