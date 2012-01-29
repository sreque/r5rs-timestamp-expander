(module timestamp-expander racket
  (provide (all-defined-out))

  (struct ts-syntax (symbol timestamp)
          #:transparent
          #:property prop:custom-write (lambda (ts port ignored)
                                         (fprintf port "~a:~a" 
                                                  (ts-syntax-symbol ts) 
                                                  (ts-syntax-timestamp ts))))
  (struct parser-state (syntax env)
          #:transparent)
  
  (define (pstate-defined? pstate symbol)
    (hash-has-key? (parser-state-env pstate) symbol))
  
  (define (timestamp-syntax syntax timestamp)
    (match syntax 
      [(list elems ...) (map (lambda (elem) (timestamp-syntax elem timestamp))
                             elems)]
      [(ts-syntax _ _) syntax]
      [other (if (symbol? other) 
                 (ts-syntax other timestamp) 
                 other)]))
  
  (define (unstamp-syntax syntax)
    (match syntax
      [(list elems ...) (map (lambda (elem) (unstamp-syntax elem)) elems)]
      [(ts-syntax id _) id]
      [other other]))
  
  (define (macro-expand syntax)
    (define time 0)
    (define (stamp-and-increment stx)
      (define result (timestamp-syntax stx time))
      (set! time (add1 time))
      result)
    (define init-stx (stamp-and-increment syntax))
    (define init-pstate (parser-state init-stx (hasheq 'or or-macro 'and and-macro)))
    (define (reduce-binding-construct pstate)
      (match pstate
        [(parser-state (list (ts-syntax 'lambda _) rest ...) _)
         (handle-lambda pstate)]
        [(parser-state syntax env) 
         (expand-main (parser-state (rewrite-as-lambda syntax) env))]))
    (define (handle-lambda pstate)
      (match pstate
        [(parser-state (list (ts-syntax 'lambda time) (list args ...) body1 body-rest ...) env);todo: abstract teasing apart of args and body
         (cons (ts-syntax 'lambda time) 
               (cons args 
                     (expand-main (parser-state 
                                   (cons body1 body-rest) 
                                   (foldl (lambda (v accum) (hash-set accum v #f)) env args)))))]))
    (define (handle-macro-application pstate)
      (match pstate
        [(parser-state (list (ts-syntax sym time) rest ...) env)
         (let* ([macro (get-macro pstate)]
                [expanded-syntax (stamp-and-increment (macro (cons sym rest)))])
           #;(printf "before: ~a\n" (cons sym rest))
           #;(printf "after: ~a\n" expanded-syntax)
           (expand-main (parser-state expanded-syntax env)))]))
    (define (expand-main pstate)
      #;(printf "expanding ~a\n" (parser-state-syntax pstate))
      (match pstate
        [(? binding-construct?)
         (reduce-binding-construct pstate)]
        [(? quote-expr?)
         (unstamp-syntax (parser-state-syntax pstate))]
        [(? macro-application?)
         (handle-macro-application pstate)]
        [(parser-state (list stuff ...) env)
         (map (lambda (elem) 
                (expand-main (parser-state elem env))) 
              stuff)]
        [(parser-state other env) other]))
    (define expanded (expand-main init-pstate))
    (alpha-rename (parser-state expanded (hasheq))))
  
  (define (alpha-rename pstate)
    #;(printf "~a -- ~a\n" (parser-state-syntax pstate) (parser-state-env pstate))
    (define (mylambda? pstate)
      (match pstate
        [(parser-state (list (ts-syntax 'lambda time) (list args ...) body1 body-rest ...) env)
         (not (hash-ref env (ts-syntax 'lambda time) #f))]
        [other #f]))
    (match pstate
      [(? mylambda?)
       (let* ([stx (parser-state-syntax pstate)] 
              [args (cadr stx)]
              [body (caddr stx)]
              [new-env (foldl 
                        (lambda (id accum) 
                          (hash-set accum id (gensym (ts-syntax-symbol id)))) 
                        (parser-state-env pstate)
                        args)])
         #;(printf "lambda args: ~a\n" args)
         `(lambda ,(alpha-rename (parser-state args new-env)) 
            ,(alpha-rename (parser-state body new-env))))]
      [(parser-state (list stuff ...) env)
       (map (lambda (elem) (alpha-rename (parser-state elem env))) stuff)]
      [(parser-state (ts-syntax sym time) env)
       (let ([new-name (hash-ref env (ts-syntax sym time) #f)])
          (if new-name new-name sym))]
      [(parser-state other env) other]))
       
  (define (get-macro pstate)
    (match pstate
      [(parser-state (list (ts-syntax sym time) rest ...) env)
       (let ([value (hash-ref env (ts-syntax sym time) #f)])
         (if value value (hash-ref env sym #f)))]
      [_ #f]))
  
  (define (macro-application? pstate)
    (let ([macro (get-macro pstate)])
      (if macro #t #f)))
             
         
  (define (quote-expr? pstate)
    (match pstate
      [(parser-state (list (ts-syntax 'quote time) rest ...) env)
       (not (hash-has-key? env (ts-syntax 'quote time)))]
      [_ #f]))
  
  (define (and-macro syntax)
    (match syntax
      [(list 'and) #t]
      [(list 'and test) test]
      [(list 'and test1 test-rest ...)
       `(if ,test1 (and ,@test-rest)) #f]))
  
  (define (or-macro syntax)
    (match syntax
      [(list 'or) #f]
      [(list 'or test) test]
      [(list 'or test1 test-rest ...)
       `(let ([v ,test1]) (if v v (or ,@test-rest)))]))
  
  (define variable-binding-constructs '(let let* letrec do lambda define))
  (define syntax-binding-constructs '(let-syntax letrec-syntax define-syntax))
  (define binding-constructs (append variable-binding-constructs syntax-binding-constructs))
  
  (define (binding-construct? pstate)
    (match (parser-state-syntax pstate)
      [(list (ts-syntax head ts) tail ...)
       (and (member head binding-constructs) 
            (not (pstate-defined? pstate head)))]
      [_ #f]))

  (define (rewrite-as-lambda syntax)
    (match syntax
      [(list (ts-syntax 'let time) rest ...)
       (rewrite-let-as-lambda syntax)]
      [(list (ts-syntax 'let* time) rest ...)
       (rewrite-let*-as-lambda syntax)]
      [(list (ts-syntax 'letrec time) rest ...)
       (rewrite-letrec-as-lambda syntax)]))
  
  (define (rewrite-let-as-lambda syntax)
    (match syntax
      [(list (ts-syntax 'let time) (list (list name val) ...) body1 body-rest ...)
       `((,(ts-syntax 'lambda time) (,@name) ,body1 ,@body-rest) ,@val)]
      [(list (ts-syntax 'let time) tag  (list (list name val) ... ) body1 body-rest ...)
       (rewrite-letrec-as-lambda 
        `((,(ts-syntax 'letrec time) ([,tag (lambda (,@name) ,body1 ,@body-rest)])
            tag) ,@val))]))
  
  (define (rewrite-letrec-as-lambda syntax)
    (match syntax
      [(list (ts-syntax 'letrec time) (list (list var val) ...) body ...)
       (let* ([gen-symbols (map (lambda (s) (gensym s)) var)]
             [temp-vars (map (lambda (gs val) (list gs val)) gen-symbols val)]
             [init-vars (map (lambda (sym) (list sym #f)) var)]
             [set-exprs (map (lambda (sym gs) `(set! ,sym ,gs)) var gen-symbols)])
         (rewrite-let-as-lambda 
          `(,(ts-syntax 'let time) (,@init-vars) 
             ,(rewrite-let-as-lambda 
                `(,(ts-syntax 'let time) (,@temp-vars)
                  ,@set-exprs ,@body)))))]))
  
  (define (rewrite-let*-as-lambda syntax)
     (match syntax
       [(list (ts-syntax 'let* time) (list) body1 body-rest ...)
        `(begin ,body1 ,@body-rest)]
       [(list (ts-syntax 'let* time) (list (list names vals) ...) body1 body-rest ...)
        (foldr (lambda (name val accum)
                 `((,(ts-syntax 'lambda time) (,name) ,accum) ,val))
               (cons 'begin (cons body1 body-rest))
               names
               vals)]))
  )
