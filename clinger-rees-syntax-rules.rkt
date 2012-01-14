(module clinger-rees-expander racket
  (require racket/unsafe/ops)
  (provide (except-out (all-defined-out) format))
  
  
  ;Temporary hack for performance. Ideally, we create two versions of the matcher module:
  ;  one that gives detailed error reporting, and 
  ;  one that is used for performance. 
  ;The performant one would be used when doing normal matching. 
  ;When a a macro entirely fails to match on an invocation, 
  ; the other version would be invoked to provide a detailed error message to the user.
  (define-syntax format
    (syntax-rules ()
      [(_ arg1 arg-rest ...) arg1]))

  (define-syntax cfor
  (syntax-rules ()
    [(_ (var init cond incr) code ...)
     (let loop ([var init])
       (when cond
         (begin code ... (loop incr))))]))
  
  (define-syntax raise-syntax-error#
    (syntax-rules ()
      [(_ syntax msg)
       (raise
        (syntax-error
         msg
         (current-continuation-marks)
         syntax))]))
  
  (struct syntax-rule (matcher rewriter regular-ids def-env id-depths))  
  ;def-env is the environment at the time of definition
  ;it needs to be combined with the pattern env and the rewrite-env
  ;id-set is the set of all identifiers that appear in an output pattern that are not pattern variables
  ;used to create rewrite-env
  ;all identifiers have to be rewritten, whether they are free or bound, because they may be rebound by the macro
  (struct output-generator (generator def-env id-set ellipses-env) 
    #:transparent)
  
  ;Should we create a separate data type for environments to restrict operations on them?
  (define (env-lookup env id)
    (hash-ref env id (denotation id)))
  
  (define (merge-envs env1 env2)
    (for/fold ((result env1))
      (((k v) (in-hash env2)))
      (hash-set result k v)))
  
  (struct denotation (id) #:transparent)
  
  (define (make-incrementer)
    (define counter 0)
    (values 
     (λ () counter) 
     (λ () (define result counter) (set! counter (add1 counter)) result)))
  
  (define (make-id-registrar)
    (define-values (num-getter num-incr) (make-incrementer))
    (define register (hash))
    (values 
     (λ () register)
     num-getter
     (λ (id)
       (when (hash-has-key? register id)
         (raise-syntax-error#
          id
          (format "duplicate pattern id detected: ~a" id)))
       (define new-idx (num-incr)) 
       (set! register (hash-set register id new-idx)) 
       new-idx)))
  
  ;This function assumes for performance reasons that both matcher-list and syntax-list are proper lists of the same length.
  (define (merge-match-results-lazy matcher-list syntax-list use-env pvec)
    (let loop ([rem-matchers matcher-list]
               [rem-syntax syntax-list])
      (if (null? rem-matchers)
          (void)
          (let ([match-result ((unsafe-car rem-matchers) (unsafe-car rem-syntax) use-env pvec)])
            (if (pattern-mismatch? match-result)
                match-result
                (loop
                 (unsafe-cdr rem-matchers)
                 (unsafe-cdr rem-syntax)))))))
  
  #;(define (merge-match-results seq)
    (let/ec break
      (for/fold ((cur-env (hash)))
        ((result (in-list seq)))
        (if (pattern-mismatch? result)
            (break result)
            (merge-envs cur-env result)))))
  
  ;foldl that requires the input lists to all be the same size
  ;this is not currently used
  (define (foldl-exact proc init . lists)
    (define (invoke-loop cur-value arg-lists)
      (loop (apply proc 
                   (cons cur-value 
                         (for/list ([v (in-list arg-lists)]) (car v)))
                   (for/list ([v (in-list arg-lists)]) (cdr v)))))
    
    (define (loop cur-value lists)
      (if (memv '() lists)
          (begin
            (for ([v (in-list lists)])
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
    (make-pattern-struct pattern-identifier vector-idx)
    (make-pattern-struct literal-identifier)
    (make-pattern-struct fixed-list sub-patterns)
    (make-pattern-struct improper-list sub-patterns tail-pattern)
    (make-pattern-struct ellipses-list sub-patterns tail-pattern)
    ;make fixed vector
    ;make ellipse vector
    (make-pattern-struct datum))
  
  (begin
    (make-template-struct template-identifier ellipses-depth vector-idx)
    (make-template-struct ellipses-template inner-template num-ellipses num-outer-ellipses id-depths anchor-ids)
    (make-template-struct template-list sub-templates)
    (make-template-struct improper-template-list sub-templates tail-template)
    ;make template vector
    (make-template-struct template-datum))
  
  (struct pattern-mismatch (pattern syntax msg)
    #:transparent)
  
  ;version of length that works on improper lists, giving them the same length as if they were proper
  (define (proper-length ls)
    (when (not (or (cons? ls) (null? ls)))
      (raise "invalid argument: must be a cons or null"))
    (let loop ([count 0] [cur ls])
      (match cur
        [(cons _ rest) (loop (add1 count) rest)]
        [else count])))
  
  (define (split-or-void _xs _point)
    (let loop ([xs _xs] [point _point])
      (cond
        [(zero? point)
         (values null xs)]
        [(cons? xs)
         (let-values ([(x y) (loop (cdr xs) (sub1 point))])
                (if (void? x)
                    (values x y)
                    (values (cons (unsafe-car xs) x) y)))]
        [else
         (values (void) (void))])))
  
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
       (let-values ([(syntax point) (values _syntax _point)])
         #;(with-handlers 
             ((exn:fail:contract? 
               (λ (v) 
                 (if (or (cons? syntax) (null? syntax))
                     (pattern-mismatch _pattern syntax (format "Syntax list length too short: ~a" syntax))
                     (pattern-mismatch _pattern syntax (format "syntax not a list: ~a" syntax))))))
           (let-values ([(x y) (split-at syntax point)])
             body1 body-rest ...))
         (define-values (x y) (split-or-void syntax point))
         (if (void? x)
             (if (or (cons? syntax) (null? syntax))
                     (pattern-mismatch _pattern syntax (format "Syntax list length too short: ~a" syntax))
                     (pattern-mismatch _pattern syntax (format "syntax not a list: ~a" syntax)))
             (begin body1 body-rest ...)))]))
  
  ;This would be nested inside of match-merge-static if I took the time to figure out how
  (define-syntax match-merge-static-helper  
    (syntax-rules ()
      [(_ result) result]
      [(_ lresult lexpr1 lexpr-rest ...) 
       (let ([lresult1 lexpr1])
         (if (pattern-mismatch? lresult1) lresult1
             (match-merge-static-helper merged lexpr-rest ...)))]))
  
  ;Lazily evaluates expressions and merges them, failing early on a pattern mismatch         
  (define-syntax match-merge-static
    (syntax-rules ()
      [(_ expr1) expr1]
      [(_ expr1 expr-rest ...)
       (let ([result1 expr1])
         (if 
          (pattern-mismatch? result1) 
          result1
          (match-merge-static expr-rest ...)))]))
  
  (define (improper-length ls check-up-to)
    (let loop ([cur ls] [count 0])
      (match cur
        [(cons _ rest) (if (>= count check-up-to) (add1 count) (loop rest (add1 count)))]
        ['() count]
        [else (- (add1 count))])))
  ;matches all values in pattern-list against all syntax elements of syntax-list. 
  ;Both should be lists of the same size.
  (define (multi-match parent-pattern matcher-list matcher-list-length syntax-list use-env pvec)
    (define len (improper-length syntax-list matcher-list-length))
    (cond 
      [(< len 0)
       (pattern-mismatch parent-pattern syntax-list (format "syntax not a proper list: ~a" syntax-list))]
      [(not (eqv? matcher-list-length len))
       (pattern-mismatch (input-pattern-source parent-pattern) syntax-list 
                              (format "arity mismatch\n  pattern:~a\n  syntax:~a" parent-pattern syntax-list))]
      [else
       (merge-match-results-lazy matcher-list syntax-list use-env pvec)]))
  
  ;TODO cache this in ellipses-list so we don't recalculate it over and over
  (define (pattern-ids-in-pattern _pattern)
    (let loop ([pattern _pattern]
               [result (set)])
      (match pattern
        [(pattern-identifier id idx) (set-add result id)]
        [(ellipses-list source sub-patterns ellipses-pattern)
         (loop ellipses-pattern 
               (loop (fixed-list source sub-patterns) result))]
        [(improper-list source sub-patterns end-pattern)
         (loop end-pattern 
               (loop (fixed-list source sub-patterns) result))]
        [(fixed-list _ sub-patterns)
         (for/fold ([r result])
                 ((p (in-list sub-patterns)))
                 (loop p r))]
        [else result])))
  
  (define (make-matcher pattern def-env pattern-indexes num-pattern-ids)
    (define-syntax recur (syntax-rules () [(_ p) (make-matcher p def-env pattern-indexes num-pattern-ids)]))
    (match pattern
      [(pattern-identifier id idx) (lambda (syntax ignored pvec) (unsafe-vector*-set! pvec idx syntax) (void))]
      [(literal-identifier id)
       ;should we verify that the input syntax is an identifier here?
       (define literal-denotation (env-lookup def-env id))
       (lambda (syntax use-env pvec)
         (define arg-denotation (env-lookup use-env syntax))
         (if (equal? literal-denotation arg-denotation)
             (void)
             (pattern-mismatch pattern syntax 
                               (format "Syntax does not match literal identifier or the denotations are different: ~a vs ~a" literal-denotation arg-denotation))))]
      ;TODO reimplement ellipses to be the same as with templates
      ;Instead of having an ellipses-list, have an ellipses sub-pattern
      [(ellipses-list _ sub-patterns ellipses-pattern)
       (define sub-matchers 
         (for/list ([p (in-list sub-patterns)]) (recur p)))
       (define ellipses-matcher (recur ellipses-pattern))
       (define pattern-ids (pattern-ids-in-pattern ellipses-pattern))
       (define indexes (for/list ([id (in-set pattern-ids)]) (hash-ref pattern-indexes id)))
       (define sub-pattern-length (length sub-patterns))
       (define temp-vec (make-vector num-pattern-ids))
       (lambda (syntax use-env pvec)
         (split-or-fail 
          ((input-pattern-source pattern) syntax sub-pattern-length)
          (fixed-syntax variable-syntax)
          #;(printf "fixed=syntax=~a variable-syntax=~a split-point=~a all-syntax=~a\n" fixed-syntax variable-syntax sub-pattern-length syntax)
          (if (or (cons? variable-syntax) (null? variable-syntax))
              (match-merge-static 
               (merge-match-results-lazy sub-matchers fixed-syntax use-env pvec)
               (ellipses-match ellipses-matcher variable-syntax use-env pvec temp-vec indexes))
              (pattern-mismatch (input-pattern-source pattern) variable-syntax (format "syntax not a list: ~a" syntax)))))]
      [(improper-list _ sub-patterns end-pattern)
       (define sub-matchers 
         (for/list ([p (in-list sub-patterns)]) (recur p)))
       (define tail-matcher (recur end-pattern))
       (define fixed-length (length sub-patterns))
       (lambda (syntax use-env pvec)
         (split-or-fail 
          ((input-pattern-source pattern) syntax fixed-length)
          (fixed-syntax end-syntax)
          (match-merge-static
           (merge-match-results-lazy sub-matchers fixed-syntax use-env pvec)
           (tail-matcher end-syntax use-env pvec))))]
      [(fixed-list _ sub-patterns) 
       (define sub-matchers 
         (for/list ([p (in-list sub-patterns)]) (recur p)))
       (define num-matchers (length sub-matchers))
       (lambda (syntax use-env pvec)
         (multi-match pattern sub-matchers num-matchers syntax use-env pvec))]
      [(datum datum)
       (lambda (syntax ignored ignored2)
         (if (equal? datum syntax)
             (void)
             (pattern-mismatch datum syntax "Syntax does not match literal datum")))]))
  
  ;matches a matcher against all elements of syntax and returns an extended environment
  ;based on those matches. Match values are concatenated into lists appropriately.
  (define (ellipses-match matcher syntax use-env pvec temp-vec indexes)
    (for ([idx (in-list indexes)]) (unsafe-vector*-set! pvec idx '()))
    (let/ec break
      (let loop ((rem-syntax syntax))
        (when (cons? rem-syntax)
          (loop (unsafe-cdr rem-syntax))
          (let ([match-result (matcher (unsafe-car rem-syntax) use-env temp-vec)])
            (if (pattern-mismatch? match-result)
              (break match-result)
              (for ([idx (in-list indexes)]) 
                (unsafe-vector*-set! pvec idx (cons (unsafe-vector*-ref temp-vec idx) (unsafe-vector*-ref pvec idx)))))))
        (void))))
  
  (struct syntax-error exn:fail (syntax)
    #:transparent)
  
  ;this will return a pattern object
  ;duplicate pattern variable usage will not be detected directly by this function for now.
  ;As an alternative, this function could return both the matcher and the results of computed-ellipses-nesting,
  ;which would also verify no pattern variable duplication for free
  (define (parse-transformer-pattern syntax literal-identifiers)
    (define-values (get-register get-register-size add-to-register) (make-id-registrar))
    (define pattern (parse-transformer-pattern-recursive syntax literal-identifiers add-to-register))
    (values pattern (get-register) (get-register-size)))
  
  (define (parse-transformer-pattern-recursive syntax literal-identifiers add-to-register)
    (define-syntax recur
      (syntax-rules ()
        [(_ _syntax) (parse-transformer-pattern-recursive _syntax literal-identifiers add-to-register)]))
    (define (parse-list parsed-stack remaining-list)
      (match remaining-list
        ['()
         (fixed-list syntax (reverse parsed-stack))]
        [(cons first rest)
         (cond           
           [(eqv? first '...) 
            (if (not (empty? rest))
                (raise-syntax-error#
                 syntax
                 "ellipses must occur at the end of a syntax list")
                (match parsed-stack
                  [(cons car-parsed-stack cdr-parsed-stack)
                   (ellipses-list syntax (reverse cdr-parsed-stack) car-parsed-stack)]
                  [else
                   (raise (syntax-error 
                           "ellipses must occur after a pattern inside of a syntax list"
                           (current-continuation-marks)
                           syntax))]))]
           [else
            (parse-list 
             (cons (recur first) parsed-stack)
             rest)])]
        [else
         (improper-list
          syntax
          (reverse parsed-stack)
          (recur remaining-list))]))
    (cond
      [(or (cons? syntax) (null? syntax))
       (parse-list '() syntax)]
      [(symbol? syntax)
       (if (set-member? literal-identifiers syntax)
           (literal-identifier syntax)
           (pattern-identifier syntax (add-to-register syntax)))]
      [(syntax-datum? syntax)
       (datum syntax)]
      [else (raise-syntax-error# 
             syntax
             (format "Unrecognized syntax type: ~a" syntax))]))
  
  (define (syntax-datum? syntax)
    (or (string? syntax) (char? syntax) (number? syntax) (boolean? syntax)))
  
  (define (merge-hashes a b resolver)
    (for/fold ([result a])
      (((k b-value) (in-hash b)))
      (define a-value (hash-ref result k (void)))
      (define value (if (void? a-value) b-value (resolver a-value b-value)))
      (hash-set result k value)))
  
  (define (merge-hash-of-sets a b) (merge-hashes a b set-union))
  
  ;returns a map from all pattern ids in a template to a set of depth requests for that template
  (define (find-pattern-ids predicate template)
    (define result
      (let loop ([t template]
                 [ids (hash)])
        (match t
          [(template-identifier id depth idx)
           (if  (predicate idx)
               (hash-set ids idx (set-add (hash-ref ids idx (set)) depth))
               ids)]
          [(ellipses-template _ inner en oen more-ids anchor-ids)
           ;if an inner template has an outer-depth of 0, then it has no anchor ids.
           ;this is a hack, but it works for now.
           ;if we ever have to call find-pattern-ids on a top-level template, which we don't right now,
           ;we would need to pass in another boolean to differentiate from outermost ellipses templates, which can have an outer depth of 0 and anchori ids.
           (if (zero? oen) ids (merge-hash-of-sets ids more-ids))]
          [(template-list _ xs)
           (for/fold ((accum ids))
             ((x (in-list xs)))
             (loop x accum))]
          [(improper-template-list source xs tail)
           (loop tail (loop (template-list source xs) ids))]
          [(template-datum _) ids])))
    result)
  
  ;multiply a value v c times, where multiplication is cons and 0 is '(). Results are undefined if c is negative.
  (define (list-mult v c)
    (let loop ((cur null) (rem c))
      (if (zero? rem)
          cur
          (loop (cons v cur) (sub1 rem)))))
  
  (define (parse-transformer-template syntax pattern-nestings pattern-indeces #:outer-ellipses-nesting (outer-ellipses-nesting 0))
    (define-syntax recur (syntax-rules () 
                           [(_ s n) (parse-transformer-template s pattern-nestings pattern-indeces #:outer-ellipses-nesting n)]
                           [(_ s) (recur s outer-ellipses-nesting)]))
    (define (parse-list parsed-stack remaining-list)
      #;(printf "  parse-list: ~a ~a\n" parsed-stack remaining-list)
      (match remaining-list
        ['()
         (template-list syntax (reverse parsed-stack))]
        [(cons first _rest)
         (cond
           [(eqv? '... first)
            (raise (syntax-error 
                    "ellipses must occur after a pattern inside of a syntax list"
                    (current-continuation-marks)
                    syntax))]
           [else
            (define-values (ellipses-count rest)
              (let loop ([ellipses-count 0]
                         [rest _rest])
                (match rest
                  [(cons '... __rest)
                   (loop (add1 ellipses-count) __rest)]
                  [else
                   (values ellipses-count rest)])))
            (define new-outer-ellipses-nesting (+ outer-ellipses-nesting ellipses-count))
            (define inner-template
              (recur first new-outer-ellipses-nesting))
            (define template
              (if (zero? ellipses-count)
                  inner-template
                  (let* ([id-depths (find-pattern-ids (λ (idx)  (>= idx 0)) inner-template)]
                         [anchor-id-finder (λ (id-depths) 
                                             (for/fold ([result (set)])
                                               (((id depths) (in-hash id-depths)))
                                               (if (set-member? depths (unsafe-vector*-ref pattern-nestings id))
                                                   (set-add result id)
                                                   result)))]
                        [anchor-ids (anchor-id-finder id-depths)])
                    (when (set-empty? anchor-ids)
                      #;(printf" no anchors for ~a outer-nesting=~a\n" (cons (output-template-source inner-template) (list-mult '... ellipses-count)) outer-ellipses-nesting)
                      ;this is a hack, but it works(TM).
                      ;an inner template is valid if would work by itself not nested inside any other ellipses templates.
                      ;In that case, we mutate some variables to allow the template to pretend that it really is not nested inside other ellipses templates.
                      (set! id-depths
                            (for/hash
                                ([(id depths) (in-hash id-depths)]) (values id (for/set ((depth (in-set depths))) (- depth outer-ellipses-nesting)))))
                      (set! anchor-ids (anchor-id-finder id-depths))
                      (set! outer-ellipses-nesting 0)
                      (when (set-empty? anchor-ids)
                        (raise-syntax-error#
                         inner-template
                         (format "template contains no identifiers whose template depth matches its corresponding pattern depth. template-id-depths=~a patter-id-depths=~a template=~a" 
                                 id-depths pattern-nestings (cons (output-template-source inner-template) (list-mult '... ellipses-count))))))
                    (ellipses-template 
                     (cons (output-template-source inner-template) (list-mult '... ellipses-count))
                     inner-template
                     ellipses-count
                     outer-ellipses-nesting
                     id-depths
                     anchor-ids))))
            (parse-list
             (cons template parsed-stack) rest)])]
        [else
         (improper-template-list
          syntax
          (reverse parsed-stack)
          (recur remaining-list outer-ellipses-nesting))]))
      #;(printf "parser-transformer-template: ~a list=~a\n" syntax (list? syntax))
    (cond
      [(or (cons? syntax) (null? syntax))
       (parse-list '() syntax)]
      [(symbol? syntax)
       (define idx (hash-ref pattern-indeces syntax -1))
       (if (and (>= idx 0) (> (unsafe-vector*-ref pattern-nestings idx)  outer-ellipses-nesting))
           (raise-syntax-error#
            syntax
            (format "Ellipses nesting of identifier ~a in template is less than its pattern nesting. pattern-nesting=~a, template-nesting=~a." syntax (hash-ref pattern-nestings syntax 0) outer-ellipses-nesting))
           (template-identifier syntax outer-ellipses-nesting idx))]
      [(syntax-datum? syntax)
       (template-datum syntax)]
      [else (raise-syntax-error# 
             syntax
             (format "Unrecognized type for syntax: ~a" syntax))]))
  
  ;Computes how many ellipses apply to each identifier in a pattern object.
  ;Throws a syntax-error if a duplicate variable use is detected.
  ;This function serves two purposes for now because both purposes involve almost identical work.
  (define (compute-ellipses-nesting top-pattern num-pattern-ids)
    (define result (make-vector num-pattern-ids (void)))
    (define (dfs pattern ellipses-level)
      (match pattern
        [(pattern-identifier id idx) 
         ;we check for this condition when building the register now
         #;(if (not (void? (unsafe-vector*-ref result idx)))
             (raise (syntax-error 
                     (format "Duplicate identifier '~a' detected in pattern" id)
                     (current-continuation-marks)
                     (input-pattern-source top-pattern)))
             (unsafe-vector*-set! result idx ellipses-level))
         (unsafe-vector*-set! result idx ellipses-level)]
        [(literal-identifier _) (void)]
        [(improper-list syntax sub-patterns end-pattern)
         (dfs (fixed-list syntax sub-patterns) ellipses-level)
         (dfs end-pattern ellipses-level)]
        [(ellipses-list syntax sub-patterns end-pattern)
         (dfs (fixed-list syntax sub-patterns) ellipses-level)
         (dfs end-pattern (add1 ellipses-level))]
        [(fixed-list syntax sub-patterns)
         (for ([sub-pattern sub-patterns]) (dfs sub-pattern ellipses-level))]
        [(datum _) (void)]))
    (dfs top-pattern 0)
    result)
  
  ;finds identifiers that aren't pattern identifiers
  ;Really this function conceptually puts all identifiers in a list and then filters them based on the predicate.
  (define (find-regular-ids template predicate)
    (define (dfs t result)
      (match t
        [(template-identifier id depth idx)
         (if (< idx 0)
             (set-add result id)
             result)]
        [(ellipses-template _ inner _ _ _ _)
         (dfs inner result)]
        [(template-list _ inner-templates)
         (for/fold ([r result]) ((sub-t (in-list inner-templates))) (dfs sub-t r))]
        [(improper-template-list _ ts tail)
         (for/fold ((r (dfs tail result))) ((t (in-list ts))) (dfs t r))]
        [(template-datum _) result]))
    (dfs template (set)))
  
  ;flattens a fixed number of levels of a list.
  ;Does not handle arbitrary s-expressions.
  ;Assumes the number of levels actually exists
  ;depth should be >= 0.
  
  ;I made this code more forgiving than I'd like to match the behavior of Racket's macro expander
  ; if we reach an empty list while recurring we succeed with an empty list even though strictly we should fail.
  (define (flatten# lst depth)
    #;(printf "flattening ~a ~a levels\n" lst depth)
    (define (recur k lst depth)
      (match lst
        [(cons first rest)    
         (define sub-k
           (if (null? rest)
               k
               (lambda () (recur k rest depth))))
         (body sub-k first (sub1 depth))]
        [else (k)]))
    (define (body k lst depth)
      #;(printf "flatten#-body lst=~a depth=~a\n" lst depth)
      (if 
       (<= depth 0)
       (let loop ([cur lst])
         (match cur
           [(cons first rest)
            (cons first (loop rest))]
           [else (k)]))
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
  
  ;Creates a new list by replicating references to smaller, matching the structure of larger.
  ;Replicates recursively based on the value of depth.
  ;If depth is 1, the list is to map larger with smaller, otherwise map larger to a recursive call with depth - 1
  (define (replicate smaller larger depth)
    #;(printf "replicating ~a using ~a depth=~a\n" smaller larger depth)
    (when (negative? depth)
      (error (format "invalid replicate depth: ~a" depth)))
    (define (unchecked smaller larger depth)
      (cond
        [(zero? depth) smaller]
        [(eqv? 1 depth) (map (λ (v) smaller) larger)]
        [else (map (λ (v) (unchecked smaller v (sub1 depth))) larger)]))
    (unchecked smaller larger depth))

  ;checks that each value in xss is of the same length as the rest
  (define (check-lengths xss source)
    (match xss
      [(cons first rest)
       (let ([l1 (length first)])
         (for ([xs (in-list rest)])
           (unless (eqv? l1 (length xs))
             (raise-syntax-error#
              source
              (format "template values length mismatch: values=~a\nsyntax=~a" xss source))))
         l1)]
      [else (void)]))
  
  ;Checks that a value is true and raises an error if it is not.
  (define-syntax assert
    (syntax-rules ()
      [(_ _v)
       (let ([v _v])
         (unless v (error "assertion failure")))]
      [(_ _v _m)
       (let ([v _v])
         (unless v (error (format "assertion failure: ~a. Message: ~a" v _m))))]))
  
  ;A much fancier version of check-lengths that deals with the fact that some of the values in vals have not yet been replicated to match the values of their anchor ids.
  ;It also recursively calls itself until ellipses-count is 0
  ;The result is undefined if ellipses-count is < 1, or if vals and rep-depths are of differing length
  ;source is just for debugging purposes
  ;vals are the actual values to compare the lengths of
  ;rep-depths are the depths the corresponding values will be replicated.
  ;Note, this function is causing a signicant(~15-25% in one case) increase in macro expansion time
  ;Without it certain illegal macros would be considered legal, though I don't think any terrible things would happen if they did became legal.
  (define (check-lengths-with-rep-depths source vals rep-depths ellipses-count)
    #;(printf "vals=~a rep-depths=~a ellipses-count=~a\n" vals rep-depths ellipses-count)
    (define no-reps
      (for/fold ([result '()])
        ((v (in-list vals)) (depth rep-depths))
        (if (eqv? depth 0) (cons v result) result)))
    (define lengths (check-lengths no-reps source))
    (when (and (> ellipses-count 1) (> lengths 0))
      (define new-rep-depths (for/list ([v (in-list rep-depths)]) (if (eqv? v 0) 0 (sub1 v))))
      (let loop ([child-cdrs vals] 
                 [vals-left (sub1 lengths)])
        (define child-cars 
          (foldr (λ (lst depth result) (cons (if (eqv? depth 0) (car lst) lst) result))
                 '()
                 child-cdrs 
                 rep-depths))
        (check-lengths-with-rep-depths source child-cars new-rep-depths (sub1 ellipses-count))
        (when (> vals-left 0)
          (define new-child-cdrs 
            (foldr 
             (λ (lst depth result) (cons (if (eqv? depth 0) (cdr lst) lst) result))
             '()
             child-cdrs 
             rep-depths))
          (loop new-child-cdrs (sub1 vals-left))))))
  
  ;transposes a list of lists.
  (define (list-transpose xss)
    (apply map list xss))
  ; converts a template struct into a function of the form 
  ; identifier-substitution-map -> syntax.
  (define (make-rewriter template pattern-depths pattern-indexes)
    (define (recur template) (make-rewriter template pattern-depths pattern-indexes))
    (define rewriter-fuser 
      (match-lambda
        ((? ellipses-template?) append)
        (else cons)))
    (define (list-fuser rewriters fusers base-case-function)
      (lambda (sub-map pvec)
        (foldr 
         (lambda (r f a) (f (r sub-map pvec) a))
         (base-case-function sub-map pvec) rewriters fusers)))
    #;(printf "making rewriter for template: ~a\n" template)
    (match template
      [(template-identifier id depth idx)
       (if (< idx 0)
           (lambda (sub-map pvec) (hash-ref sub-map id))
           (lambda (sub-map pvec) #;(printf "sub-map=~a id=~a depth=~a\n" sub-map id depth) (unsafe-vector*-ref (unsafe-vector*-ref pvec idx) 0)))]
      [(ellipses-template source inner ellipses-count outer-ellipses-count id-depths anchor-ids)
       ;I apologize in advance to anyone, including my future self, that has to maintain this chunk of code.
       ;Oh my gosh I'm having to maintain it right now!
       (define inner-rewriter (recur inner))
       (define anchor-list (set->list anchor-ids))
       (define anchor-idx (car anchor-list))
       (define anchor-depth (unsafe-vector*-ref pattern-depths anchor-idx))
       (define anchor-cur-depth (- anchor-depth outer-ellipses-count))
       (define rem-depth (- anchor-cur-depth outer-ellipses-count))
       (assert (>= rem-depth 0) (format "rem-depth=~a anchor-id=~a anchor-depth=~a outer-ellipses-count=~a ellipses-count=~a" rem-depth anchor-id anchor-depth outer-ellipses-count ellipses-count))
       (lambda (sub-map pvec)
         (define anchor-val (unsafe-vector*-ref (unsafe-vector*-ref pvec anchor-idx) (- anchor-depth outer-ellipses-count)))
         ;Each value is either null or a list containing four values:
         ;the identifier to be acted upon, it's requested depth, its replication-depth value (0 if not replicated), 
         ;and the function that will actually do the work on the value  
         ;the id's new depth for sub-expressions can be inferred to be requested-depth - ellipses-count + replication-depth
         ;it is built and flattened our map of sets, and apply is used to do the flattening.
         ;Finally, if the value is null we simply ignore the corresponding id-depth pair for now.
         (define sub-map-additions
           (filter 
            (λ (v) (not (null? v)))
            (apply append 
                   (for/list
                       ([(id-idx depths) (in-hash id-depths)])
                     (define anchor-id? (set-member? anchor-ids id-idx))
                     (define pattern-depth (unsafe-vector*-ref pattern-depths id-idx))
                     (for/list ([depth (in-set depths)])
                       (define cur-depth (- depth outer-ellipses-count))
                       (define depth-diff (- cur-depth pattern-depth))
                       (assert (>= cur-depth ellipses-count))
                       (cond
                         ;anchor ids are the simplest. They get pre-flattened by ellipses-count - 1, since we later call map, which implicitly flattens by 1.
                         [(and anchor-id? (eqv? depth pattern-depth))
                          #;(printf "found anchor-id ~a at depth=~a\n" id depth)
                          (list id-idx (- pattern-depth outer-ellipses-count) 0 (λ (vs) (flatten# vs (sub1 ellipses-count))))]
                         [(>= depth-diff 0)
                          ;this means we will need to do some replicating based on an anchor id
                          (if (<= depth-diff ellipses-count)
                              ;because of multiple ellipses, we will replicate and flatten as one step
                              ;TODO refactor code common between the anchor-id case and this case
                              (list id-idx pattern-depth depth-diff (λ (vs) (flatten# (replicate vs anchor-val depth-diff) (sub1 ellipses-count))))
                              ;I honestly don't remember what this branch means.
                              '())]
                         [else
                          ;We are treating it the same as an anchor id. How is it not an anchor id at this point? Should we error out here instead?
                          (list id-idx (- depth outer-ellipses-count) 0 (λ (vs) (flatten# vs (sub1 ellipses-count))))]))))))
         ;the values before flattening/replicating. This will only be used to verify that lengths are the same for all values.
         (define pre-xss
           (for/list ([v (in-list sub-map-additions)])
             (match-define (cons id  (cons depth _)) v)
             (unsafe-vector*-ref (unsafe-vector*-ref pvec id) depth)))
         (define rep-depths
           (for/list ([v (in-list sub-map-additions)]) (unsafe-car (unsafe-cdr (unsafe-cdr  v)))))
         ;this is where we verify that value lengths are correct. We could check this after flattening and only use check-lengths.
         ;This would be faster but would make certain edge case macro invocations valid that should result in errors, as we lose information during the flattenings.
         (check-lengths-with-rep-depths source pre-xss rep-depths ellipses-count)
         (define xss
           (for/list ([v (in-list sub-map-additions)])
             (match-define (list id depth rep-depth func) v)
             (func (unsafe-vector*-ref (unsafe-vector*-ref pvec id) depth))))
         #;(printf "xss=~a sub-map-additions=~a id-depths=~a sub-map=~a anchor-ids=~a source=~a\n" xss sub-map-additions id-depths sub-map anchor-ids source)
         (define transposed-xss (list-transpose xss))
         ;This is where we invoke our inner template multiple times using augmented substitution maps.
         (define values-to-restore
           (for/list ([add-info (in-list sub-map-additions)]
                      #:when (not (null? add-info)))
             (if (null? add-info)
                 (void)
                 (let ()
                   (match-define (list id depth rep-depth f) add-info)
                   (define depth-val (+ (- depth ellipses-count) rep-depth))
                   #;(list (unsafe-vector*-ref (unsafe-vector*-ref pvec id) depth-val) id depth-val)
                   (unsafe-vector*-ref (unsafe-vector*-ref pvec id) depth-val)))))
         (define result 
           (for/list
               ([args (in-list transposed-xss)])
             (for ([arg (in-list args)]
                   [add-info (in-list sub-map-additions)])
               (when (not (null? add-info))
                 (let ()
                   (match-define (list id depth rep-depth f) add-info)
                   (unsafe-vector*-set! (unsafe-vector*-ref pvec id) (+ (- depth ellipses-count) rep-depth) arg))))
             (inner-rewriter sub-map pvec)))
         (for ([old-value (in-list values-to-restore)]
               [add-info (in-list sub-map-additions)])
           (match-define (list id depth rep-depth f) add-info)
           (define depth-val (+ (- depth ellipses-count) rep-depth))
           (unsafe-vector*-set! (unsafe-vector*-ref pvec id) depth-val old-value))
             result)]
      [(template-list _ inner-templates)
       (list-fuser
        (for/list ([t (in-list inner-templates)]) (recur t))
        (for/list ([t (in-list inner-templates)]) (rewriter-fuser t))
        (lambda (ignore ignore2) '()))]
      [(improper-template-list source sub-templates tail-template)
       (define tail-rewriter (recur tail-template))
       (list-fuser
        (for/list ([t (in-list sub-templates)]) (recur t))
        (for/list ([t (in-list sub-templates)]) (rewriter-fuser t))
        tail-rewriter)]
      [(template-datum d) 
       (lambda (ignored ignore2) d)]))
  
  ;Parses a pattern/template pair under an environment and literal id list
  ; (syntax, syntax, hash-table, set) -> rule
  (define (parse-syntax-rule pattern-syntax template-syntax def-env literal-ids)
    #;(display "==================\n")
    #;(printf "  pattern=~a\n" pattern-syntax)
    #;(printf "  template=~a\n" template-syntax)
    (define-values (pattern register register-size) (parse-transformer-pattern pattern-syntax literal-ids))
    (define ellipses-nesting (compute-ellipses-nesting pattern register-size))
    (define template (parse-transformer-template 
                      template-syntax
                      ellipses-nesting
                      register))
    (syntax-rule 
     (make-matcher pattern def-env register register-size)
     (make-rewriter template ellipses-nesting register)
     (find-regular-ids template (λ (v) (< (template-identifier-vector-idx v) 0))#;(λ (v) (not (hash-has-key? ellipses-nesting v))))
     def-env
     ellipses-nesting))
  
  ;Parses a syntax-rules expression into a list of rules
  ;(syntax, hash-table) -> list[rule]
  (define (parse-syntax-rules syntax def-env)
    (define (parse-literals syntax)
      (when (not (list? syntax))
        (error (format "Expected a syntax-rules literals list, got a non-list value ~a" syntax)))
      (for/fold ((result (set)))
        ((lit (in-list syntax)))
        (when (not (symbol? lit))
          (error (format "non-symbol value inside of syntax-rules literals list: ~a" lit)))
        (when (set-member? result lit)
          (error (format "literals list contains a duplicate of identifier ~a" lit)))
        (set-add result lit)))
    (define (parse-pattern/template-pairs syntax literal-ids)
      (when (not (list? syntax))
        (error (format 
                "malformed syntax-rules syntax. Got a non-list value for the list of pattern-template pairs: ~a" 
                syntax)))
      (for/list ([s (in-list syntax)])
        (match s
          [(list pattern template)
           (match pattern
             [(cons pattern-first pattern-rest)
              (if (not (symbol? pattern-first))
                (error (format "syntax-rules pattern must begin with an identifier: ~a" s))
                (parse-syntax-rule pattern-rest template def-env literal-ids))]
             [else
              (error (format "syntax-rules pattern must be a non-empty list. Got: ~a" pattern))])]
          [else
           (error (format "Expected a list of length 2 containing a pattern and template, got: ~a" syntax))])))
    (match syntax
      [(cons head (cons literal-ids pairs))
       (when (not (equal? (env-lookup def-env head) (denotation 'syntax-rules)))
         (error (format "the first identifier of syntax-rules syntax must be the symbol 'syntax-rules: got ~a" head)))
       (define literals (parse-literals literal-ids))
       (define rules (parse-pattern/template-pairs pairs literals))
       rules]
      [else
       (error "Expected a syntax-rules form containing a literals list and 0 or more pattern/template pairs. Got: ~a" syntax )]))
  
  ;Converts a list of rules into a single macro transformer function
  ; (list[rule]) -> (syntax, hash-table, hash-table) -> (syntax, hash-table, hash-table)
  ;hash-table no. 1 represents the environment at the time of macro use
  ;hash-table no. 2 represents a mapping from re-written symbols to their original value
  (define (make-macro-transformer syntax-rules)
    ;assume at this point that syntax-rules is a non-empty list of syntax-rule structs.
    (define (rewrite rule syntax use-env pattern-env orig-sym-env)
      (match-define (syntax-rule matcher rewriter regular-ids def-env pattern-nestings) rule)
      (define pvec (make-vector (unsafe-vector*-length pattern-nestings)))
      (define num-pids (unsafe-vector*-length pattern-nestings))
      (cfor (i 0 (< i num-pids) (add1 i))
            (unsafe-vector*-set! pvec i (make-vector (add1 (unsafe-vector*-ref pattern-nestings i)))))
      (define fresh-regular-env
        (for/hash ([id (in-set regular-ids)])
          (values id (gensym id))))
      (define macro-env fresh-regular-env)
      (cfor (i 0 (< i num-pids) (add1 i))
            (unsafe-vector*-set! (unsafe-vector*-ref pvec i) (unsafe-vector*-ref pattern-nestings i) (unsafe-vector*-ref pattern-env i))) 
      (define new-use-env
        (for/fold ((result use-env))
          (([id new-id] (in-hash fresh-regular-env)))
          (define cur-binding (hash-ref def-env id (void)))
          (define binding (if (void? cur-binding)
                              (denotation id)
                              cur-binding))
          #;(printf "expansion aliasing ~a to ~a\n" new-id binding)
          (hash-set result new-id binding)))
      (define new-orig-sym-env
        (for/fold ([result orig-sym-env])
          (([id new-id] (in-hash fresh-regular-env)))
          (hash-set result new-id id)))
      (define new-syntax (rewriter macro-env pvec))
      (values new-syntax new-use-env new-orig-sym-env))
    (if (null? syntax-rules)
        (lambda (syntax use-env orig-sym-env) (error "no rules provided, which means the macro invocation always fails"))
        (lambda (syntax use-env orig-sym-env)
          (define rest-syntax (cdr syntax))
          (let loop 
            ((match-failures '())
             (rem-rules syntax-rules))
            (match rem-rules
              [(cons rule rest)
               (let ()
                 (match-define (syntax-rule matcher rewriter regular-ids def-env pattern-nestings) rule)
                 (define pvec (make-vector (unsafe-vector*-length pattern-nestings)))
                 (define match ((syntax-rule-matcher rule) rest-syntax use-env pvec))
                 (if (pattern-mismatch? match)
                      (loop (cons match match-failures) rest)
                      (rewrite rule syntax use-env pvec orig-sym-env)))]
              [else
               (error (format "match failed\nsyntax: ~a\nmatch errors: ~a" syntax (reverse match-failures)))])))))
  
  ;Turns syntax, with the given environment, into a function of the following signature:
    ; syntax -> environment -> environment -> (syntax, environment, environment)
      ;any valid scheme syntax
      ;the use environment
      ;the environment mapping transformed symbols to their original user-specified symbols (needed to implement quote)
      ;returns the transformed syntax, the extended use environment, and the extended original symbol environment
  (define (parse-syntax-transformer syntax def-env)
    (make-macro-transformer
     (parse-syntax-rules syntax def-env)))
  )
