(module clinger-rees-parser racket
  (provide (except-out (all-defined-out) format))
  #;(provide (all-defined-out))
  (require 
   racket/unsafe/ops
   "clinger-rees-syntax-rules.rkt")
  ;our goal in this file is to define methods that can handle all the primitive forms that aren't easily defined in terms of another form
  ;  top-level define, local define, define-syntax, let-syntax, letrec-syntax, lambda
  ;(define (syntax-definition? form)

  ;See notes to format macro in clinger-rees-syntax-rules.rkt
  (define-syntax format
    (syntax-rules ()
      [(_ arg1 arg-rest ...) arg1]))
  
  (define (verify-binding-form-shape binding-form)
    (match binding-form
      [(list (? symbol?) _) (void)]
      [else
       (raise-syntax-error#
        binding-form
        "binding form must be a list of length 2 whose first value is an identifier")])
    ;parse-syntax-rules already throws exceptions with human-readable error messages. 
    ;I should factor that code out into a separate verify method
    #;(when not (syntax-definition? (cadr binding-form))        
      (raise-syntax-error#
       (cadr binding-form)
       "second value of binding form must be a syntax definition")))
  
  (define (verify-syntax-binding-shape syntax form-name)
    (match syntax
      [(cons first (cons _ _))
       (when (not (list? first))
         (raise-syntax-error#
          first
          (format "first argument to ~a must be a list" form-name)))
       (for ([binding-form (in-list first)])
         (verify-binding-form-shape binding-form))]
      [else
       (raise-syntax-error# 
         syntax
         (format "expected at least 2 arguments to ~a, got ~a" form-name (length syntax)))]))
    
  (define (reduce-let-syntax syntax def-env)
    (verify-syntax-binding-shape syntax "let-syntax")
    (let* ([extended-env 
            (for/fold ([result def-env])
              ((binding-form (in-list (unsafe-car syntax))))
              (define id (unsafe-car binding-form))
              (define cur-binding (hash-ref def-env id (void)))
              (define orig-sym (if (void? cur-binding) id (unsafe-cdr cur-binding)))
              #;(printf "extending env with let-macro ~a\n" (unsafe-car binding-form))
              (hash-set result 
                        (unsafe-car binding-form)
                        (cons (parse-syntax-transformer (unsafe-car (unsafe-cdr binding-form)) def-env) orig-sym)))])
      (values (unsafe-cdr syntax) extended-env)))
 
  (define (reduce-letrec-syntax syntax def-env)
    (define (make-delayed-transformer ref)
      (lambda (syntax use-env)
        ((unsafe-unbox ref) syntax use-env)))
    (define (make-premature-use-macro symbol)
      (lambda (syntax use-env)
      (raise-syntax-error#
       syntax
       "Cannot recursively invoke a macro until its binding is complete")))
    (verify-syntax-binding-shape syntax "letrec-syntax")
    (let*-values 
        ([(binding-forms) (unsafe-car syntax)]
           [(ref-map extended-env)
            (for/fold
                ([_ref-map (hash)]
                 [_extended-env def-env])
                ((binding-form (in-list binding-forms)))
              (define id (unsafe-car binding-form))
              (define ref (box (make-premature-use-macro first)))
              (define delayed-macro (make-delayed-transformer ref))
              (define cur-binding (hash-ref def-env id (void)))
              (define orig-sym (if (void? cur-binding) id (unsafe-cdr cur-binding)))
              (values
               (hash-set _ref-map id ref)
               (hash-set _extended-env id (cons delayed-macro orig-sym))))])
      (for ([binding-form (in-list binding-forms)])
        (define ref (hash-ref ref-map (unsafe-car binding-form)))
        (define transformer (parse-syntax-transformer (unsafe-car (unsafe-cdr binding-form)) extended-env))
        (unsafe-set-box! ref transformer))
      (values (unsafe-cdr syntax) extended-env)))
  
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
        (match rem
          [(cons first rest)
           (loop rest (add-id ids first))]
          ['() ids]
          [else (add-id ids rem)])))
  
  ;verifies the shape of the lambda form and returns the set of identifiers the form binds
  ; if this function ever became part of something other than an expander, it could be modified
  ; to return information about the formals 
  (define (verify-lambda-shape syntax)
    (match syntax
      [(cons formals (cons _ _))
       (verify-lambda-formals formals)]
      [else
       (raise-syntax-error#
        syntax
        "lambda form requires at least an argument list and one expression")]))
  
  ;extends the environment and returns the list of body expressions of the lambda with the new environment
  (define (reduce-lambda syntax env)
    (define ids (verify-lambda-shape syntax))
    (define (extended-env)
      (for/fold ([result env])
        ((id (in-set ids)))
        (define new-id (gensym id))
        #;(printf "aliasing ~a to ~a with denotation ~a\n" id new-id (hash-ref env id (denotation id)))
        (hash-set result id (cons new-id id))))
    (values (unsafe-cdr syntax) (extended-env)))
  
  ;verifies that a define is shaped correctly and then returns the identifier the define binds
  (define (verify-define-shape syntax)
    (match syntax
      [(cons head (cons head2 rest))
       (match head
         [(? symbol?)
          (if (null? rest)
              head
              (raise-syntax-error#
            syntax
            "define form with one variable as its first argument must have exactly one expression following the identifier"))]
         [(cons car-head _)
          car-head]
         ['{}
          (raise-syntax-error#
            syntax
            "The first argument to a define form, if a list, must contain at least one identifier")]
         [else
          (raise-syntax-error# 
             syntax
             "First argument to a define form must be an identifier or a list of an identifier prepended to a formals list")])]
      [else
       (raise-syntax-error#
        syntax
        "define form does not have the required arguments")]))
  
  (define (parse-define syntax)
    (define id (verify-define-shape syntax))
    (define body 
      (if (symbol? (unsafe-car syntax))
          (unsafe-car (unsafe-cdr syntax))
          `(lambda ,(unsafe-cdr (unsafe-car syntax)) ,@(unsafe-cdr syntax))))
    (values id body))
  
  (define (reduce-define syntax env)
    (define-values (id body) (parse-define syntax))
    (values (hash-set env id id) body))
  
  ;returns a symbol if the identifier is bound to a runtime value
  ;returns a procedure if the identifier is bound to a syntax transformer
  ;returns void if the identifier is unbound
  
  (define get-local-denotation-default (cons (void) (void)))
  (define (get-denotation symbol top-env local-env #:default (default (void)))
    (define local-denotation (unsafe-car (hash-ref local-env symbol get-local-denotation-default)))
    (match local-denotation
      [(? void?) ;unbound locally
       (values (hash-ref top-env symbol default) symbol)] ;therefore get top level binding
      [(denotation id)  ;bound to the top-level denotation of another identifier
       (values (hash-ref top-env id default) id)] ;therefore get the top-level denotation of the other identifier
      [else (values local-denotation symbol)])) ;bound locally, therefore return local binding
  
  (define (unquote-syntax syntax use-env)
    (match syntax
      ['() syntax]
      [(cons first rest)
       (cons (unquote-syntax first use-env) (unquote-syntax rest use-env))]
      [(? vector?) (for/vector ((s (in-vector syntax))) (unquote-syntax s use-env))]
      [(? symbol?) (let ([den (hash-ref use-env syntax (void))]) (if (void? den) syntax (unsafe-cdr den)))]
      [else             syntax]))
  
  ;This function should be private, if I took the time to learn how to do that with Racket modules.
  ;it returns a tuple of the denotation of a symbol and a predicate that can be used to test if the denotation is a particular keyword
  ;the denotation will either be void, a symbol, or a procedure
  ;the predicate will return true if the denotation is a symbol that matches the passed-in keyword, and false otherwise 
  (define (get-denotation-and-keyword-predicate symbol top-env local-env)  
    (define-values  (denotation denotation-symbol) (get-denotation symbol top-env local-env))
    #;(printf "denotation symbol for ~a is ~a. Denotation is ~a\n" symbol denotation-symbol denotation)
    (define bound? (not (void? denotation)))
    (define denotes-keyword? (if bound?
                                 (lambda (keyword) #f)
                                 (lambda (keyword) (eqv? denotation-symbol keyword))))
    (values denotation denotes-keyword?))
  
  ;This is the hack I use to get working the feature where macros can expand to internal defines
  ;this is basically a piece of syntax together with an environment and a reverse symbol map for quoting
  (define-struct partially-expanded (syntax local-env) #:transparent)
  
  ;expand a list of body syntax expressions.
  ;this function returns a list of expressions, just as it takes as input a list of expressions
  ;this function handles defines using letrec.
  ;it attempts to allow one to splice defines with a begin expression and use macros that expand to defines or begins with defines.
  (define (expand-inner-body-syntax syntax-list top-env local-env)
    ;There's a good chance this code, meant to allow the non-r5rs feature of having macros expand into internal defines, could be broken by design.
    ;it has to expand and rewrite everything all the way because we throw away the syntactic environment at the end of this function call
    (define (handle-macro-use macro use-syntax local-env)
      (define-values (new-syntax new-local-env) (macro use-syntax local-env))
      (match new-syntax
        [(cons (? symbol? symbol) _)
         (define-values (denotation denotes-keyword?) (get-denotation-and-keyword-predicate symbol top-env new-local-env))
         (cond
           [(procedure? denotation)
            #;(printf "invoking macro ~a on ~a\n" (unsafe-car new-syntax) new-syntax)
            (handle-macro-use denotation new-syntax new-local-env)]
           [else
            (partially-expanded new-syntax new-local-env)])]
        [else
         #;(printf "after using macro ~a to expand ~a to ~a, recursively epxanding with new-local-env=~a\n" denotation use-syntax new-syntax new-local-env)
          (partially-expanded new-syntax new-local-env)]))
    (define-syntax app-pred
      (syntax-rules ()
        [(_ _v)
         (let  ([v _v]) (and (cons? v) (symbol? (unsafe-car v))))]))
    (define-syntax super-app-pred
      (syntax-rules ()
        [(_ _v)
         (let ([v _v])
           (or (app-pred v) (and (partially-expanded? v) (app-pred (partially-expanded-syntax v)))))]))
    #;(define-syntax app-pred (syntax-rules () [(_ v) (match v [(cons (? symbol?) _) #t] [else #f])]))
    #;(define super-app-pred (λ (v) (or (app-pred v) (and (partially-expanded? v) (app-pred (partially-expanded-syntax v))))))
    (define (loop defines-list rem-list)
      (define-syntax no-more-defines
        (syntax-rules ()
          [(_)
           (begin
             (define-values (new-syntax new-local-env)
               (rewrite-inner-body-syntax defines-list rem-list local-env))
             #;(printf "new syntax: ~a\n" new-syntax)
             (for/list ([v (in-list new-syntax)]) (expand-inner-syntax v top-env new-local-env)))]))
      (match rem-list 
        ['()
         (raise-syntax-error#
          syntax-list
          (format "body must contain at least one expression that is not a define. defines-list=~a" defines-list))]
        [(cons (? super-app-pred app-expr-pre) cdr-rem-list)
         (define pre-expanded? (partially-expanded? app-expr-pre))
         (define app-expr (if pre-expanded? (partially-expanded-syntax app-expr-pre) app-expr-pre))
         (define le 
           (if pre-expanded?
               (partially-expanded-local-env app-expr-pre)
               local-env))
         (define inner-expander
           (if pre-expanded?
               (lambda (s te _le) (expand-inner-syntax s te le))
               expand-inner-syntax))
         (define syntax-wrapper
           (if pre-expanded?
               (λ (v) (partially-expanded v le))
               (λ (v) v)))
         (define symbol (unsafe-car app-expr))
         (define-values (denotation denotes-keyword?) (get-denotation-and-keyword-predicate symbol top-env le))
         (cond
           [(denotes-keyword? 'define)
            (define-values (id value) (parse-define (unsafe-cdr app-expr)))
            (loop (cons (list id (syntax-wrapper value)) defines-list) cdr-rem-list)]
           [(denotes-keyword? 'begin)            
            (loop defines-list (append (map syntax-wrapper (unsafe-cdr app-expr)) cdr-rem-list))]
           [(procedure? denotation)
            #;(printf "invoking macro ~a on ~a\n" symbol app-expr)
            (loop defines-list (cons (handle-macro-use denotation app-expr le) cdr-rem-list))]
           [else (no-more-defines)])]
        [else (no-more-defines)]))
    #;(printf "Expanding body syntax: ~a\n" syntax-list)
    (loop '() syntax-list))
  
  ;implement syntactic rewrite of internal defines as a macro for hygiene
  (define rewrite-inner-body-macro
    (parse-syntax-transformer
     '(syntax-rules ()
        [(macro ((symbol def) ...) body ...)
         ((letrec
             ((symbol def) ...) body ...))]) 
     (hash)))
  
  ;implement syntactic rewrite of internal begin statements as a macro for hygiene
  ;for now, we are going to treat internal begin's as a special form and not use this macro at all.
  (define rewrite-inner-begin-macro
    (parse-syntax-transformer 
     '(syntax-rules ()
        [(macro statements ...) ((lambda () statements ...))])
     (hash)))
  
  (define wrap-with-begin-macro
    (parse-syntax-transformer
     '(syntax-rules ()
        [(macro statements ...) (begin statements ...)]) (hash))) 
  
  ;instead of calling rewrite-inner-begin-macro directly, use this function instead
  ;it is expected that the defines-list is in reverse order of the original declaration
  (define (rewrite-inner-body-syntax inverse-defines-list body-list local-env)
    #;(printf "defines-list: ~a\n" (reverse inverse-defines-list))
    (if (null? inverse-defines-list) (values body-list local-env)
        (rewrite-inner-body-macro `(,(gensym 'inner-body-macro) ,(reverse inverse-defines-list) ,@body-list) local-env)))

  ;expand a piece of syntax that is not at the top level and not at the beginning of a body
  ;this means that define and define-syntax is illegal
  ;returns the expanded syntax. Since no identifiers can be bound, there is no need to return a new local-env
  (define (expand-inner-syntax syntax top-env local-env #:at-top-level (at-top-level #f))
    (define-syntax recur
      (syntax-rules ()
        [(_ s le) (expand-inner-syntax s top-env le #:at-top-level at-top-level)]))
    (define-syntax handle-procedure-application
      (syntax-rules ()
        [(_)
        (for/list ([child-syntax (in-list syntax)]) (recur child-syntax local-env))]))
    ;this will reduce the syntax to its body and expanded environment and then expand the environment
    ;The return value is a single piece of syntax, either a begin or letrec expression
    (define (handle-local-syntax-extension reducer)
      (define-values (body-syntax extended-env) (reducer (unsafe-cdr syntax) local-env))
      (define expanded-forms (expand-inner-body-syntax  body-syntax top-env extended-env))
      (if (not (null? (unsafe-cdr expanded-forms)))
          (cons 'begin expanded-forms)
          (unsafe-car expanded-forms)))
    (define (handle-symbol-application)
      (define symbol (unsafe-car syntax))
      (define-values (denotation denotes-keyword?) (get-denotation-and-keyword-predicate symbol top-env local-env))
      #;(printf "applied symbol=~a denotation=~a eqv-quote?=~a\n" symbol denotation (denotes-keyword? 'quote))
      (cond
        [(or (denotes-keyword? 'define) (denotes-keyword? 'define-syntax))
         (if at-top-level
             (if (denotes-keyword? 'define)
                 (let*-values ([(id value) (parse-define (unsafe-cdr syntax))])
                   `(define ,id ,(recur value local-env)))
                 syntax)
             (raise-syntax-error#
              syntax
              (format "~a illegal except at the top level and at the beginning of a body expression" symbol)))]
        [(denotes-keyword? 'begin)
         (cons 'begin (expand-inner-body-syntax (unsafe-cdr syntax) top-env local-env))]
        [(denotes-keyword? 'let-syntax)
         (handle-local-syntax-extension reduce-let-syntax)]
        [(denotes-keyword? 'letrec-syntax)
         (handle-local-syntax-extension reduce-letrec-syntax)]
        [(denotes-keyword? 'lambda)
         (define-values (body-syntax new-local-env) (reduce-lambda (unsafe-cdr syntax) local-env))
         `(lambda
           ,(if (empty? (cadr syntax)) '() (recur (cadr syntax) new-local-env)) ;we expand inner syntax here simply to do simple identifier rewrites
          ,@(expand-inner-body-syntax  body-syntax top-env new-local-env))]
        [(denotes-keyword? 'quote)
         (match syntax
           [(list sym arg) (unquote-syntax syntax local-env)]
           [else
            (raise-syntax-error#
              syntax
              "Quote form must contain exactly one argument")])]
        [(procedure? denotation) 
         #;(printf "invoking macro ~a on ~a\n" symbol syntax)
         (define-values (expanded-syntax expanded-local-env) 
           (denotation syntax local-env))
         (recur expanded-syntax expanded-local-env)]
        [else (handle-procedure-application)]))
    #;(printf "Expanding inner syntax: ~a\n  local-env=~a\n" syntax local-env)
    (cond
      [(null? syntax)
       (raise-syntax-error#
          syntax
          "illegal empty list")]
      [(cons? syntax)
       (define head (unsafe-car syntax))
       (if (symbol? head)
           (handle-symbol-application)
           (if (cons? head)
               (handle-procedure-application)
               (raise-syntax-error#
                syntax
                "head value cannot possibly be a procedure")))]
      [(symbol? syntax)
       (define-values (denotation ignored) (get-denotation syntax top-env local-env #:default syntax))
       #;(printf "unapplied symbol:xs value=~a user-symbol=~a denotation=~a\n" syntax user-sym denotation)
       (cond
         [(symbol? denotation)
          denotation]
         [(procedure? denotation)
           (raise-syntax-error#
            syntax
            (format "Identifier ~a is bound to a macro: which must be applied to arguments" syntax))]
         [else
          (raise-syntax-error#
           syntax
           (format "Unrecognized denotation: ~a.\n This suggests a bug in the macro expander." denotation))])]
      [(vector? syntax)
       (raise-syntax-error#
        syntax
        "support for vector macros not yet added")]
      [(syntax-datum? syntax) syntax]
      [(partially-expanded? syntax)
       (match-define (partially-expanded s le) syntax)
       (recur s le)]
      [else 
       (raise-syntax-error#
        syntax
        (format "Unrecognized syntax type for the following syntax: ~a" syntax))]))
  
  #;(define expand-inner-syntax-orig expand-inner-syntax)
  #;(set! expand-inner-syntax (lambda (syntax top-env local-env #:at-top-level (at-top-level #f))
                              (define result (expand-inner-syntax-orig syntax top-env local-env #:at-top-level at-top-level))
                              (printf "before expansion: ~a\nafter expansion: ~a\n" syntax result)
                              result))
  
  ;expand a top level form and return (new-top-env expand-exp)
  (define (expand-top-level-form top-env sexp)
    (define (expand-default)
      (expand-inner-syntax sexp top-env (hash) #:at-top-level #t))
    (match sexp
      [(cons (? symbol? symbol) contents)
       (define-values (denotation denotes-keyword?) (get-denotation-and-keyword-predicate symbol top-env (hash)))
       (cond
         [(denotes-keyword? 'define)
          (define-values (id body) (parse-define contents))
          (define new-top-env (hash-set top-env id id))
          (values new-top-env `(define ,id ,(expand-inner-syntax body new-top-env (hash))))]
         [(denotes-keyword? 'define-syntax)
          (define rest contents)
          (match contents
            [(list (? symbol? keyword) transformer-spec)
             (let*
                 ;so the way things are implemented, recursive macros break if the identifier the macro was bound to is changed to refer to something else.
                 ; I don't know if it is good or standards compliant to do so, but this code ensures that
                 ; identifiers generated by the macro that would refer to the macro always do so, regardless of whether 
                 ; the top-level meaning of the identifier is changed.
                 ([ref (box (void))]
                  [macro-wrapper (λ (s le) ((unsafe-unbox ref) s le))]
                  [new-top-env (hash-set top-env keyword macro-wrapper)]
                  [macro (parse-syntax-transformer transformer-spec (hash keyword (cons macro-wrapper keyword)))])
               (unsafe-set-box! ref macro)
               (values (hash-set top-env keyword macro) (void)))]
            [else
             (raise-syntax-error#
             sexp
             "define syntax form should contain exactly one keyword and one transformer spec")])]
         [(procedure? denotation)
          (expand-top-level-form top-env (expand-default))]
         [else
          (values top-env (expand-default))])]
       [else
        (values top-env (expand-default))]))
  
  ;expand the output of a reader, which I expect to be a list of forms
  (define (expand-program orig-top-env syntax-list)
    (define top-env orig-top-env)
    (define expanded 
      (reverse 
       (for/fold
           ([a '()])
         ((s (in-list syntax-list)))
         
          (define-values (new-env expanded-syntax) (expand-top-level-form top-env s))
          (define accum (if (void? expanded-syntax) a (cons expanded-syntax a))) 
          (set! top-env new-env)
          accum)))
    (values top-env expanded))
    
         
         
          
                           
)
