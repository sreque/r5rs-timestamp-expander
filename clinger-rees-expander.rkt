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
  
  (struct input-pattern (source) #:transparent)
  (struct output-template (source) #:transparent)
  
  (define-syntax make-pattern-struct
    (syntax-rules ()
      [(_ name syms ...) (struct name input-pattern (syms ...) #:transparent)]))
  
  (define-syntax make-template-struct
    (syntax-rules ()
      [(_ name syms ...) (struct name output-template (syms ...) #:transparent)]))
  ;how do I do an algebraic data type in Racket?
  ;This is the input pattern type's sub-types
  
  (begin
    (make-pattern-struct pattern-identifier)
    (make-pattern-struct literal-identifier)
    (make-pattern-struct fixed-list sub-patterns)
    (make-pattern-struct improper-list sub-patterns tail-pattern)
    (make-pattern-struct ellipses-list sub-patterns tail-pattern)
    ;make fixed vector
    ;make ellipse vector
    (make-pattern-struct datum))
  
  ;TODO rename to make-template-struct
  (begin
    (make-template-struct template-identifier)
    (make-template-struct ellipses-template inner-template num-ellipses pattern-ids)
    (make-template-struct template-list sub-templates)
    (make-template-struct improper-template-list sub-templates tail-template)
    ;make template vector
    (make-template-struct template-datum))
     
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
            (pattern-mismatch (input-pattern-source parent-pattern) syntax-list 
                              (format "arity mismatch\n  pattern:~a\n  syntax:~a" pattern-list syntax-list))
            (merge-match-results
             ;I would like to make this lazy with stream-map, but stream-map only takes one argument list
             (map (lambda (p s) (match-input p s def-env use-env)) pattern-list syntax-list)))))
        
  (define (match-input pattern syntax def-env use-env)
    (match pattern
      [(pattern-identifier id) (hash id syntax)]
      [(literal-identifier id)
       ;should we verify that the input syntax is an identifier here?
       (if (eqv? (env-lookup def-env id) (env-lookup use-env syntax))
           (hash)
           (pattern-mismatch pattern syntax "Syntax does not match literal identifier or the denotations are different"))]
      [(ellipses-list _ sub-patterns ellipses-pattern)
       (split-or-fail 
        ((input-pattern-source pattern) syntax (length sub-patterns))
        (fixed-syntax variable-syntax)
        (match-merge-static 
         (multi-match pattern sub-patterns fixed-syntax (hash) def-env use-env)
         (ellipses-match ellipses-pattern variable-syntax def-env use-env)))]
      [(improper-list _ sub-patterns end-pattern)
       (split-or-fail 
        ((input-pattern-source pattern) syntax (length sub-patterns))
        (fixed-syntax end-syntax)
        (match-merge-static
         (multi-match 
          pattern
          sub-patterns
          fixed-syntax
          (hash)
          def-env use-env)
         (match-input end-pattern end-syntax def-env use-env)))]
      [(fixed-list _ sub-patterns) 
       (multi-match pattern sub-patterns syntax (hash) def-env use-env)]
      [(datum datum)
       (if (eqv? datum syntax)
           (hash)
           (pattern-mismatch datum syntax "Syntax does not match literal datum"))]
  ))
  
  (define (ellipses-match pattern syntax def-env use-env)
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
         (define new-env (match-input pattern s def-env use-env))
         (if (pattern-mismatch? new-env)
             (break new-env)
             (merge cur-env new-env))))))
  
  (struct syntax-error exn:fail (syntax)
          #:transparent)
  
  ;this will return a pattern object
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
  
  (define (parse-transformer-template syntax pattern-ids env)
    (define quote-redefined? 
      (hash-has-key? env 'quote))
    (define (find-pattern-ids template)
      (define result
        (let loop ([t template]
                   [ids (set)])
          (match t
            [(template-identifier id)
             (if (set-member? pattern-ids id)
                 (set-add ids id)
                 ids)]
            [(ellipses-template _ inner _ more-ids)
             (set-union ids more-ids)]
            [(template-list _ xs)
             (for/fold ((accum ids))
               ((x xs))
               (loop x accum))]
            [(improper-template-list source xs tail)
             (loop tail (loop (template-list source xs) ids))]
            [(template-datum _) ids])))
      (if (set-empty? result)
          (raise (syntax-error 
                  "Pattern preceding an ellipses  must contain at least one identifier"
                (current-continuation-marks)
                (output-template-source template)))
          result))
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
                                (add1 (ellipses-template-num-ellipses top-parsed))
                                (ellipses-template-pattern-ids top-parsed))
                               (ellipses-template 
                                (output-template-source top-parsed)
                                top-parsed
                                1
                                (find-pattern-ids top-parsed)))
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
                    (parse-transformer-template (car rest) pattern-ids env)))]
              [else
               (parse-list 
                (cons (parse-transformer-template first pattern-ids env) parsed-stack)
                rest)]))))
    #;(printf "parser-transformer-template: ~a list=~a\n" syntax (list? syntax))
    (cond
      [(list? syntax)
       (if (and (not (empty? syntax))
                (eqv? (car syntax) 'quote)
                (not quote-redefined?))
           ;TODO verify the list is of length two
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
      
  ;Computes how many ellipses apply to each identifier in a pattern object.
  ;Throws a syntax-error if a duplicate variable use is detected.
  ;This function serves two purposes for now because both purposes involve almost identical work.
  (define (compute-ellipses-nesting top-pattern)
    (define (dfs pattern prev-seen ellipses-level)
      (match pattern
        [(pattern-identifier id) 
         (if (hash-has-key? prev-seen id)
             (raise (syntax-error 
                     (format "Duplicate identifier '~a' detected in pattern" id)
                     (current-continuation-marks)
                     (input-pattern-source top-pattern)))
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
    (dfs top-pattern (hash) 0))
  
  ;finds identifiers that aren't pattern identifiers
  (define (find-regular-identifiers template pattern-ids)
    (define (dfs t result)
      (match t
        [(template-identifier id)
         (if (set-member? pattern-ids id)
             result
             (set-add result id))]
        [(ellipses-template _ inner _ _)
         (dfs inner result)]
        [(template-list _ inner-templates)
         (for/fold ((r result)) ((_t inner-templates)) (dfs _t r))
         (foldl (lambda (sub-t r) (dfs sub-t r))  result inner-templates)]
        [(improper-template-list _ ts tail)
         (for/fold ((r (dfs tail result))) ((t ts)) (dfs t r))]
        [(template-datum _) result]))
    (dfs template (set)))

  (define (verify-template-ellipses-nesting top-template expected-nestings)
    (define (dfs template ellipses-level)
      (match template
        [(template-identifier id)
         (define expected-value (hash-ref expected-nestings id null))
         (when (and (not (null? expected-value))
                    (not (eqv? expected-value ellipses-level)))
           (raise (syntax-error
                   (format "Ellipses nesting of identifier ~a in template does not match nesting in pattern" id)
                   (current-continuation-marks)
                   (output-template-source template))))]
        [(ellipses-template source inner-template num-ellipses pattern-ids)
         (dfs inner-template (+ num-ellipses ellipses-level))]
        [(template-list source sub-templates)
         (for ([t sub-templates])
           (dfs t ellipses-level))]
        [(improper-template-list source sub-templates tail-template)
         (dfs (template-list source sub-templates) ellipses-level)
         (dfs tail-template ellipses-level)]
        [(template-datum value) #f ]))
    (dfs top-template 0))
  
  ;flattens a fixed number of levels of a list.
  ;Does not handle arbitrary s-expressions.
  ;Assumes the number of levels actually exists
  ;depth should be >= 0.
  (define (flatten# lst depth)
    (define (recur k lst depth)
      (define rest (cdr lst))
      (define sub-k
        (if (null? rest)
            k
            (lambda () (recur k rest depth))))
      (body sub-k (car lst) (sub1 depth)))
    (define (body k lst depth)
      #;(printf "flatten#-body lst=~a depth=~a\n" lst depth)
      (if 
       (<= depth 0)
       (let loop ([cur lst])
         (if (null? cur)
             (k)
             (cons (car cur) (loop (cdr cur)))))
       (recur k lst depth)))
    (if (or (null? lst) (<= depth 0))
        lst
        (body (lambda () '()) lst depth)))
                  
  ;This version is simpler but I think it has a worse
  ;asymptotic complexity than above.
  (define (flatten#-v2 l d)
    #;(if (and (list? l) (not (zero? d)))
        (apply append (map (curryr flatten2 (sub1 d)) l))
        l)
    (if (or (not (list? l)) (empty? l) (zero? d))
        l
        (append (flatten#-v2 (first l) (sub1 d)) (flatten#-v2 (rest l) d))))
        
  ; converts a template struct into a function of the form 
  ; identifier-substitution-map -> syntax.
  (define (make-rewriter template)
    (define rewriter-fuser 
      (match-lambda
        ((? ellipses-template?) append)
        (else cons)))
    (define (list-fuser rewriters fusers base-case-function)
      (lambda (sub-map)
        (foldr 
         (lambda (r f a) (f (r sub-map) a))
         (base-case-function sub-map) rewriters fusers)))
    (match template
      [(template-identifier id)
       (lambda (sub-map) (hash-ref sub-map id))]
      [(ellipses-template _ inner num-ellipses pattern-ids)
       (define inner-rewriter (make-rewriter inner)) 
       (lambda (sub-map)
         (define xss
           ;all sub-lists should be of the same length
           ;Is there any case where this might not be true that we need to check for?
           (for/list ([id pattern-ids])
             (flatten# (hash-ref sub-map id) (sub1 num-ellipses))))
         (apply 
          map 
          (lambda args 
            (define _sub-map sub-map)
            (for ([k pattern-ids]
                  [v args])
              (set! _sub-map (hash-set _sub-map k v)))
            (inner-rewriter _sub-map)) xss))]
      [(template-list _ inner-templates)
       (list-fuser
        (map (lambda (t) (make-rewriter t)) inner-templates)
        (map (lambda (t) (rewriter-fuser t)) inner-templates)
        (lambda (ignore) '()))]
      [(improper-template-list source sub-templates tail-template)
       (define tail-rewriter (make-rewriter tail-template))
       (list-fuser
        (map (lambda (t) make-rewriter t) sub-templates)
        (map (lambda (t) rewriter-fuser t) sub-templates)
        (lambda (sub-map) (tail-rewriter sub-map)))]
      [(template-datum d) 
       (lambda (ignored) d)]))
        
    )
     
