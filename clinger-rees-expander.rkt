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
  
  
  
  (define (env-lookup env id)
    (hash-ref env id id))
  
  (define (merge-envs env1 env2)
    (for/fold ((result env1))
      (((k v) env2))
      (hash-set result k v)))

  ;foldl that requires the input lists to all be the same size
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
  
  (struct input-matcher (pattern))
  (define-syntax make-input-struct
    (syntax-rules ()
      [(_ name syms ...) (struct name input-matcher (syms ...) #:transparent)]))
  
  ;how do I do an algebraic data type in Racket?
  ;This is the input matcher type's sub-types
  (begin
    (make-input-struct pattern-identifier identifier)
    (make-input-struct literal-identifier identifier)
    (make-input-struct fixed-list sub-patterns)
    (struct improper-list fixed-list (tail-pattern) #:transparent)
    (struct ellipses-list fixed-list (tail-pattern) #:transparent)
    ;make fixed vector
    ;make ellipse vector
    (make-input-struct datum))
  
  (struct pattern-mismatch (pattern syntax msg)
          #:transparent)

  (define (multi-match parent-pattern pattern-list syntax-list init-value def-env use-env)
    (define (inner-loop cur-value matcher-list syntax-list)
      (if (pattern-mismatch? cur-value)
          cur-value
          (if (null? matcher-list)
              cur-value
              
              (let ([next-value (match-input (car matcher-list) (car syntax-list) def-env use-env)])
                (if (pattern-mismatch? next-value)
                    next-value
                    (inner-loop (merge-envs cur-value next-value) (cdr matcher-list) (cdr syntax-list)))))))
    (if (not (list? syntax-list))
        (pattern-mismatch parent-pattern syntax-list "syntax not a list")
        (if (not (eqv? (length pattern-list) (length syntax-list)))
            (pattern-mismatch (input-matcher-pattern parent-pattern) syntax-list 
                              "arity mismatch")
            (inner-loop init-value pattern-list syntax-list))))
        
  (define (match-input matcher syntax def-env use-env)
    (match matcher
      [(pattern-identifier _ id) (hash id syntax)]
      [(literal-identifier _ id)
       ;should we verify that the input syntax is an identifier here?
       (if (eqv? (env-lookup def-env id) (env-lookup use-env syntax))
           (hash)
           (pattern-mismatch matcher syntax "Syntax does not match literal identifier or the denotations are different"))]
      [(ellipses-list _ sub-patterns ellipses-pattern)
       ;TODO check length here and return pattern-mismatch on bad lengths
       (define fixed-length (length sub-patterns))
       (define-values (fixed-syntax variable-syntax) (split-at syntax fixed-length))
       (define fixed-result (multi-match matcher fixed-syntax syntax def-env use-env))
       (if (pattern-mismatch? fixed-result)
           fixed-result
           (let ([variable-result (ellipses-match ellipses-pattern variable-syntax def-env use-env)])
             (if (pattern-mismatch? variable-result)
                 variable-result
                 (merge-envs fixed-result variable-result))))]
      [(improper-list _ sub-patterns end-pattern)
       ;TODO check length here and return pattern-mismatch on bad lengths
       (define-values (fixed-syntax end-syntax) (split-at syntax (length fixed-syntax)))
       (multi-match matcher (cons end-pattern sub-patterns) (cons end-syntax fixed-syntax) def-env use-env)]
      [(fixed-list _ sub-patterns) 
       (multi-match matcher sub-patterns syntax def-env use-env)]
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
        (define cur-val (hash-ref env '()))
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
    )
      
