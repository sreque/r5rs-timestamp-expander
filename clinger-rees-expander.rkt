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
  
  (struct input-matcher (source))
  (define-syntax make-input-struct
    (syntax-rules ()
      [(_ name syms ...) (struct name input-matcher (syms ...) #:transparent)]))
  
  ;how do I do an algebraic data type in Racket?
  ;This is the input matcher type's sub-types
  (begin
    (make-input-struct pattern-identifier)
    (make-input-struct literal-identifier)
    (make-input-struct fixed-list sub-patterns)
    (struct improper-list fixed-list (tail-pattern) #:transparent)
    (struct ellipses-list fixed-list (tail-pattern) #:transparent)
    ;make fixed vector
    ;make ellipse vector
    (make-input-struct datum))
  
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
  
  (struct syntax-error exn:fail (syntax msg)
          #:transparent)
  
  ;We still need to a way to report the nesting of identifiers inside ellipses lists
  ;We still need to verify that pattern variables are not used more than once
  ;We could do both of the above by analyzing the parsed pattern rather than intermingling the two.
  ;this will return a matcher object
  (define (parse-transformer-pattern syntax literal-identifiers)
    (define (parse-list parsed-stack remaining-list)
      (if (empty? remaining-list)
          (fixed-list syntax (reverse parsed-stack))
          (let ([first (car remaining-list)]
                [rest (cdr remaining-list)])
            (case first
              ['... 
               (if (not (empty? rest))
                   (raise (syntax-error syntax "ellipses must occur at the end of a syntax list"))
                   (if (empty? parsed-stack)
                       (raise (syntax-error syntax "ellipses must occur after a pattern inside of a syntax list"))
                       (ellipses-list syntax (reverse (cdr parsed-stack)) (car parsed-stack))))]
              ['|.|
               (if (or (empty? rest) (not (empty? (cdr rest))))
                   (raise (syntax-error syntax "syntax list with a . must be followed by exactly one pattern"))
                   (improper-list
                    syntax
                    (reverse parsed-stack)
                    (parse-transformer-pattern first literal-identifiers)))]
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
      [(or (string? syntax) (char? syntax) (number? syntax) (boolean? syntax))
       (datum syntax)]
      [else (raise (syntax-error syntax "Unrecognized syntax type"))]))
  
  (define (compute-ellipses-nesting top-matcher)
    (define (dfs matcher prev-seen ellipses-level)
      (match matcher
        [(pattern-identifier id) 
         (if (hash-has-key? prev-seen id)
             (raise (syntax-error 
                     (input-matcher-source top-matcher)
                     (format "Duplicate identifier ~a detected in pattern" id)))
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
             
    )
     
