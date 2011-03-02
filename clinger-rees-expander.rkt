(module clinger-rees-expander racket
  (provide (all-defined-out))
  
  ; matcher returns a pattern-env or #f if matching fails.
  ; ellipses env gives the number of ellipses affecting each identifier in the pattern
  (struct pattern-matcher (matcher ellipses-env)
          #:transparent)
  
  ;def-env is the environment at the time of definition
    ;it needs to be combined with the pattern env and the rewrite-env
  ;id-set is the set of all identifiers that appear in an output pattern that are not pattern variables
    ;used to create rewrite-env
    ;all identifiers have to be rewritten, whether they are free or bound, because they may be rebound by the macro
  (struct output-generator (generator def-env id-set ellipses-env) 
          #:transparent)
  
  ;Should we create a separate data type for environments to restrict operations on them?
  (define (env-lookup env id)
    (hash-ref env id id))
  
  (define (merge-envs env1 env2)
    (for/fold ((result env1))
      (((k v) env2))
      (hash-set result k v)))

  (define (merge-match-results seq)
    (let/ec break
      (for/fold ((cur-env (hash)))
         ((result seq))
         (if (pattern-mismatch? result)
             (break result)
             (merge-envs cur-env result)))))
  
  ;foldl that requires the input lists to all be the same size
  ;this is not currently used
  (define (foldl-exact proc init . lists)
    (define (invoke-loop cur-value arg-lists)
      (loop (apply proc 
                   (cons cur-value 
                         (map (lambda (v) (car v)) arg-lists))
                   (map (lambda (v) (cdr v)) arg-lists))))
    
    (define (loop cur-value lists)
      (if (memv '() lists)
          (begin
            (for ([v lists])
              (when (not (eqv? v '()))
                  (raise (error "lists of differing size"))))
            cur-value)
          (invoke-loop cur-value lists)))
    (invoke-loop init lists))
  
  (struct input-matcher (source) #:transparent)
  (struct output-template (source) #:transparent)
  
  (define-syntax make-input-struct
    (syntax-rules ()
      [(_ name syms ...) (struct name input-matcher (syms ...) #:transparent)]))
  
  (define-syntax make-output-struct
    (syntax-rules ()
      [(_ name syms ...) (struct name output-template (syms ...) #:transparent)]))
  ;how do I do an algebraic data type in Racket?
  ;This is the input matcher type's sub-types
  (begin
    (make-input-struct pattern-identifier)
    (make-input-struct literal-identifier)
    (make-input-struct fixed-list sub-patterns)
    (make-input-struct improper-list sub-patterns tail-pattern)
    (make-input-struct ellipses-list sub-patterns tail-pattern)
    ;make fixed vector
    ;make ellipse vector
    (make-input-struct datum))
  
  (begin
    (make-output-struct template-identifier)
    (make-output-struct ellipses-template inner-template num-ellipses)
    (make-output-struct template-list sub-templates)
    (make-output-struct improper-template-list sub-templates tail-template)
    ;make template vector
    (make-output-struct template-datum))
     
  (struct pattern-mismatch (pattern syntax msg)
          #:transparent)

  ;attempts to split a list. On failure, returns a pattern mismatch object
  ;On success, binds two passed-in identifiers to the split values and 
  ;invokes the passed in expressions, which should return either an environment
  ;or a pattern mismatch
  ;arguments: 
  ;  pattern: the syntax location where the split is occuring. Perhaps rename to source?
  ;  syntax: the actual syntax to split.
  ;  point: the index at which to split the syntax list
  ;  x: the first identifier to bind the split results to
  ;  y: the second identifier to bind the split results to
  ;  body+: the expressions to execute on a successful split
  (define-syntax split-or-fail
    (syntax-rules ()
      [(_ (_pattern _syntax _point) (x y) body1 body-rest ...)
       (let-values ([(pattern syntax point) (values _pattern _syntax _point)])
         (if (not (list? syntax))
             (pattern-mismatch pattern syntax "syntax not a list")
             (if (< (length syntax) point)
                 (pattern-mismatch pattern syntax "Syntax list length too short")
                 (let-values ([(x y) (split-at syntax point)])
                   body1 body-rest ...))))]))
  
  ;This would be nested inside of match-merge-static if I took the time to figure out how
  (define-syntax match-merge-static-helper  
    (syntax-rules ()
      [(_ result) result]
      [(_ lresult lexpr1 lexpr-rest ...) 
       (let ([lresult1 lexpr1])
         (if (pattern-mismatch? lresult1) lresult1
             (let ([merged (merge-envs lresult lresult1)])
               (match-merge-static-helper merged lexpr-rest ...))))]))
  
  ;Lazily evaluates expressions and merges them, failing early on a pattern mismatch         
  (define-syntax match-merge-static
    (syntax-rules ()
      [(_ expr1) expr1]
      [(_ expr1 expr-rest ...)
       (let ([result1 expr1])
           (if 
            (pattern-mismatch? result1) 
            result1 
            (match-merge-static-helper result1 expr-rest ...)))]))
  
  ;matches all values in pattern-list against all syntax elements of syntax-list. Both should
  ; be lists of the same size
  (define (multi-match parent-pattern pattern-list syntax-list init-value def-env use-env)
    (if (not (list? syntax-list))
        (pattern-mismatch parent-pattern syntax-list "syntax not a list")
        (if (not (eqv? (length pattern-list) (length syntax-list)))
            (pattern-mismatch (input-matcher-source parent-pattern) syntax-list 
                              (format "arity mismatch\n  pattern:~a\n  syntax:~a" pattern-list syntax-list))
            (merge-match-results
             ;I would like to make this lazy with stream-map, but stream-map only takes one argument list
             (map (lambda (p s) (match-input p s def-env use-env)) pattern-list syntax-list)))))
        
  (define (match-input matcher syntax def-env use-env)
    (match matcher
      [(pattern-identifier id) (hash id syntax)]
      [(literal-identifier id)
       ;should we verify that the input syntax is an identifier here?
       (if (eqv? (env-lookup def-env id) (env-lookup use-env syntax))
           (hash)
           (pattern-mismatch matcher syntax "Syntax does not match literal identifier or the denotations are different"))]
      [(ellipses-list _ sub-patterns ellipses-pattern)
       (split-or-fail 
        ((input-matcher-source matcher) syntax (length sub-patterns))
        (fixed-syntax variable-syntax)
        (match-merge-static 
         (multi-match matcher sub-patterns fixed-syntax (hash) def-env use-env)
         (ellipses-match ellipses-pattern variable-syntax def-env use-env)))]
      [(improper-list _ sub-patterns end-pattern)
       (split-or-fail 
        ((input-matcher-source matcher) syntax (length sub-patterns))
        (fixed-syntax end-syntax)
        (match-merge-static
         (multi-match 
          matcher
          sub-patterns
          fixed-syntax
          (hash)
          def-env use-env)
         (match-input end-pattern end-syntax def-env use-env)))]
      [(fixed-list _ sub-patterns) 
       (multi-match matcher sub-patterns syntax (hash) def-env use-env)]
      [(datum datum)
       (if (eqv? datum syntax)
           (hash)
           (pattern-mismatch datum syntax "Syntax does not match literal datum"))]
  ))
  
  (define (ellipses-match matcher syntax def-env use-env)
    ;TODO use functional vector instead?
    (define (merge cur-env new-env)
      (for/fold ((env cur-env))
        (((k v) new-env))
        (define cur-val (hash-ref env k '()))
        (hash-set env k (cons v cur-val))))
    (define (process-result result)
      (if (pattern-mismatch? result)
          result
          (for/fold ((new-result (hash)))
            (((k v) result))
            (hash-set new-result k (reverse v)))))
    (process-result 
     (let/ec break
       (for/fold ((cur-env (hash)))
         ((s syntax))
         (define new-env (match-input matcher s def-env use-env))
         (if (pattern-mismatch? new-env)
             (break new-env)
             (merge cur-env new-env))))))
  
  (struct syntax-error exn:fail (syntax)
          #:transparent)
  
  ;this will return a matcher object
  ;duplicate pattern variable usage will not be detected directly by this function for now.
  ;As an alternative, this function could return both the matcher and the results of computed-ellipses-nesting,
  ;which would also verify no pattern variable duplication for free
  (define (parse-transformer-pattern syntax literal-identifiers)
    (define (parse-list parsed-stack remaining-list)
      (if (empty? remaining-list)
          (fixed-list syntax (reverse parsed-stack))
          (let ([first (car remaining-list)]
                [rest (cdr remaining-list)])
            (case first
              ['... 
               (if (not (empty? rest))
                   (raise (syntax-error "ellipses must occur at the end of a syntax list"
                                        (current-continuation-marks)
                                        syntax))
                   (if (empty? parsed-stack)
                       (raise (syntax-error 
                               "ellipses must occur after a pattern inside of a syntax list"
                              (current-continuation-marks)
                              syntax))
                       (ellipses-list syntax (reverse (cdr parsed-stack)) (car parsed-stack))))]
              ['|.|
               (if (or (empty? rest) (not (empty? (cdr rest))))
                   (raise (syntax-error
                           "syntax list with a . must be followed by exactly one pattern"
                           (current-continuation-marks)
                           syntax))
                   (improper-list
                    syntax
                    (reverse parsed-stack)
                    (parse-transformer-pattern (car rest) literal-identifiers)))]
              [else
               (parse-list 
                (cons (parse-transformer-pattern first literal-identifiers) parsed-stack)
                rest)]))))
    (cond
      [(list? syntax)
       (parse-list '() syntax)]
      [(symbol? syntax)
       (if (set-member? literal-identifiers syntax)
           (literal-identifier syntax)
           (pattern-identifier syntax))]
      [(syntax-datum? syntax)
       (datum syntax)]
      [else (raise (syntax-error 
                    "Unrecognized syntax type" 
                    (current-continuation-marks) 
                    syntax))]))
  
  (define (syntax-datum? syntax)
    (or (string? syntax) (char? syntax) (number? syntax) (boolean? syntax)))
  
  (define (parse-transformer-template syntax env menv)
    (define quote-redefined? 
      (or
       (hash-has-key? env 'quote)
       (hash-has-key? menv 'quote)))
    (define (verify-has-identifier template)
      (let/ec break
        (define body
          (match-lambda
            ((template-identifier _) (break template))
            ((ellipses-template _ inner _) (body inner))
            ((template-list _ xs) (for ([x xs]) (body x)))
            ((improper-template-list _ xs tail)
             (for ([x xs]) (body x))
             (body tail))
            ((template-datum _) #f)))
        (body template)
        (raise (syntax-error 
                "Pattern preceding an ellipses  must contain at least one identifier"
                (current-continuation-marks)
                (output-template-source template)))))
    (define (parse-list parsed-stack remaining-list)
     #;(printf "  parse-list: ~a ~a\n" parsed-stack remaining-list)
      (if (empty? remaining-list)
          (template-list syntax (reverse parsed-stack))
          (let ([first (car remaining-list)]
                [rest (cdr remaining-list)])
            (case first
              ['...
               (if (empty? parsed-stack)
                       (raise (syntax-error 
                               "ellipses must occur after a pattern inside of a syntax list"
                              (current-continuation-marks)
                              syntax))
                       (let ([top-parsed (car parsed-stack)])
                         (parse-list 
                          (cons 
                           (if (ellipses-template? top-parsed)
                               (ellipses-template 
                                (output-template-source top-parsed)
                                (ellipses-template-inner-template top-parsed)
                                (add1 (ellipses-template-num-ellipses top-parsed)))
                               (ellipses-template 
                                (output-template-source top-parsed)
                                (verify-has-identifier top-parsed)
                                1))
                           (cdr parsed-stack)) 
                          rest)))]
              ['|.|
               (if (or (empty? rest) (not (empty? (cdr rest))))
                   (raise (syntax-error
                           "syntax list with a . must be followed by exactly one pattern"
                           (current-continuation-marks)
                           syntax))
                   (improper-template-list
                    syntax
                    (reverse parsed-stack)
                    (parse-transformer-template (car rest) env menv)))]
              [else
               (parse-list 
                (cons (parse-transformer-template first env menv) parsed-stack)
                rest)]))))
    #;(printf "parser-transformer-template: ~a list=~a\n" syntax (list? syntax))
    (cond
      [(list? syntax)
       (if (and (not (empty? syntax))
                (eqv? (car syntax) 'quote)
                quote-redefined?)
           (template-datum (cadr syntax))
           (parse-list '() syntax))]
      [(symbol? syntax)
       (template-identifier syntax)]
      [(syntax-datum? syntax)
       (template-datum syntax)]
      [else (raise (syntax-error 
                    (format "Unrecognized type for syntax: ~a" syntax) 
                    (current-continuation-marks) 
                    syntax))]))
      
  ;Computes how many ellipses apply to each identifier in a matcher object.
  ;Throws a syntax-error if a duplicate variable use is detected.
  ;This function serves two purposes for now because both purposes involve almost identical work.
  (define (compute-ellipses-nesting top-matcher)
    (define (dfs matcher prev-seen ellipses-level)
      (match matcher
        [(pattern-identifier id) 
         (if (hash-has-key? prev-seen id)
             (raise (syntax-error 
                     (format "Duplicate identifier '~a' detected in pattern" id)
                     (current-continuation-marks)
                     (input-matcher-source top-matcher)))
             (hash-set prev-seen id ellipses-level))]
        [(literal-identifier _) prev-seen]
        [(improper-list syntax sub-patterns end-pattern)
         (dfs end-pattern 
              (dfs (fixed-list syntax sub-patterns) prev-seen ellipses-level)
              ellipses-level)]
        [(ellipses-list syntax sub-patterns end-pattern)
         (dfs end-pattern 
              (dfs (fixed-list syntax sub-patterns) prev-seen ellipses-level)
              (add1 ellipses-level))]
        [(fixed-list syntax sub-patterns)
         (if (empty? sub-patterns)
             prev-seen
             (dfs (fixed-list syntax (cdr sub-patterns))
                  (dfs (car sub-patterns) prev-seen ellipses-level) ellipses-level))]
        [(datum _) prev-seen]))
    (dfs top-matcher (hash) 0))             

  (define (verify-template-ellipses-nesting top-template expected-nestings)
    (define (dfs template prev-seen ellipses-level)
      (match template
        [(template-identifier id)
         (define prev-value (hash-ref prev-seen id null))
         (define expected-value (hash-ref expected-nestings id null))
         (when (and (not (null? expected-value))
                    (not (eqv? expected-value ellipses-level)))
           (raise (syntax-error
                   (format "Ellipses nesting of identifier ~a in template does not match nesting in pattern" id)
                   (current-continuation-marks)
                   (output-template-source template))))
         (when (and (not (null? prev-value))
                    (not (eqv? prev-value ellipses-level)))
           (raise (syntax-error 
                   (format "Identifier '~a' is applied to an inconsistent number of ... in template" id)
                   (current-continuation-marks)
                   (output-template-source top-template))))
         (if (null? expected-value)
             prev-seen
             (hash-set prev-seen id ellipses-level))]
        [(ellipses-template source inner-template num-ellipses)
         (dfs inner-template prev-seen (add1 ellipses-level))]
        [(template-list source sub-templates)
         (if (empty? sub-templates)
             prev-seen
             (dfs (template-list source (cdr sub-templates))
                  (dfs (car sub-templates) prev-seen ellipses-level)
                  ellipses-level))]
        [(improper-template-list source sub-templates tail-template)
         (dfs tail-template
              (dfs (template-list source sub-templates) prev-seen ellipses-level)
              ellipses-level)]
        [(template-datum value) prev-seen]))
    (dfs top-template (hash) 0))
    )
     
