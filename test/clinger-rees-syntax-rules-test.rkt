(module clinger-rees-syntax-rules-test racket
  (provide clinger-rees-syntax-rules-test string-prefix? sym-matcher)
  (require racket 
           rackunit
           "test-utils.rkt"
           "../clinger-rees-syntax-rules.rkt")
  
  (define (check-match-success result)
    (check-pred void? result))
  
  (define (check-match-failure result)
    (pattern-mismatch? result))
  
  (define hash-empty? (compose not hash-iterate-first))
  
  ;Helper function to check that an identifier was generated from some base identifier
  (define (string-prefix? string prefix)
    (define end (string-length prefix))
    (if (< (string-length string) end)
        #f
        (let loop ([idx 0])
          (cond 
            [(>= idx end) #t]
            [(not (eqv? (string-ref string idx) (string-ref prefix idx))) #f]
            [else (loop (add1 idx))]))))
     
     ;curries the second argument of string-prefix? with (symbol->string prefix-sym)
     (define (sym-matcher prefix-sym)
       (define prefix (symbol->string prefix-sym))
       (lambda (sym)
         (string-prefix? (symbol->string sym) prefix)))
     
     (define (invert-hash the-hash)
       (for/hash ([(k v) the-hash])
         (values v k)))
     
     (define (hash-length the-hash)
       (hash-count the-hash))
     
     ;assumes all identifiers in env were renamed from symbols, and that all symbols were bound locally at the time of definition, and that the syntax definition is not recursive (let-syntax).
     ;verifies that exactly the bindings that are expected are present and are based off their parent bindings.
     (define (check-quote-env env symbols)
       (define env-reversed (invert-hash env))
       (for ([sym symbols])
         (check-pred (sym-matcher sym) (hash-ref env-reversed sym)))
       (check-equal? (length symbols) (hash-length env)))
     
     (define and-syntax
       '(syntax-rules ()
          ((and) #t)
          ((and test) test)
          ((and test1 test2 ...)
           (if test1 (and test2 ...) #f))))
  (define clinger-rees-syntax-rules-test
    (test-suite
     "clinger/rees syntax rules test"
     
     ;test matching of various pattern data (characters, booleans, numbers, strings)
     (let* ([syntax 'a]
            [p1 (datum 'a)] ;really symbols shouldn't be supported
            [p2 (datum "a")]
            [p3 (datum #\a)]
            [p4 (fixed-list '() (list (datum syntax)))]
            [register (hasheq 'a 0)]
            [pvec (make-vector 1 (void))])
       (define result ((make-matcher p1 (hasheq) register 1) syntax (hasheq) pvec))
       (check-match-success result)
       (check-pred void? (vector-ref pvec 0) (format "vector should be empty: ~a\n" pvec))
       (for ([p (list p2 p3 p4)])
         (check-match-failure ((make-matcher p (hasheq) register 1) syntax (hasheq) pvec))))
     
     ;test pattern identifier's resulting matcher matching different kinds of syntax.
     (let* ([pattern (pattern-identifier 'v 0)]
            [register (hasheq 'v 0)]
            [matcher (make-matcher pattern (hasheq) register 1)]
            [s1 'v]
            [s2 '(a b c d)]
            [s3 #(bob dole is cool)]
            [s4 #hash( (1 . 2) (3 . 4) (5 . 6))]
            [pvec (make-vector 1 (void))])
       (for ([s (list s1 s2 s3 s4)])
         (define result (matcher s (hasheq) pvec))
         (check-match-success result)
         (check-equal? s (vector-ref pvec 0))))
     
     ;test matching of a  literal identifier based on the environment
     (let* ([id 'x]
            [pattern (literal-identifier id)]
            [bad-syntaxes (list 'a "b" #\x `(,id) #(x))]
            [empty-env (hasheq)]
            [env1 (hasheq id (cons (gensym id) id))]
            [env2 (hasheq id (cons (gensym id) id))]
            [id2 'y]
            [diff-env (hasheq id2 (hash-ref env1 id))])
       (for ([env (list empty-env env1 env2)])
         (define result ((make-matcher pattern env (hasheq) 0) id env (vector)))
         (check-match-success result))
       (for ([s bad-syntaxes])
         (check-match-failure ((make-matcher pattern empty-env (hasheq) 0) s empty-env (vector))))
       (for ([e1 (list empty-env env1 env2)]
             [e2 (list env1 env2 empty-env)])
         (check-match-failure ((make-matcher pattern e1 (hasheq) 0) id e2 (vector))))
       (check-match-success ((make-matcher pattern env1 (hasheq) 0) id2 diff-env (vector)))
       (check-match-failure ((make-matcher pattern env1 (hasheq) 0) id diff-env (vector)))
       (check-match-failure ((make-matcher pattern env2 (hasheq) 0) id2 diff-env (vector))))
     
     ;test matching of fixed list against syntax lists of the same and different sizes and with different contents
     (let* ([syntax '(a b c d e f g)]
            [big-syntax (append syntax '(h))]
            [small-syntax (take syntax (sub1 (length syntax)))]
            [wrong-syntax (map (lambda (s) (if (eqv? s 'd) 'z s)) syntax)]
            [pattern (fixed-list syntax (map (lambda (s) (datum s)) syntax))]
            [matcher (make-matcher pattern (hasheq) (hasheq) 0)])
       (define good-result (matcher syntax (hasheq) (vector)))
       (check-match-success good-result)
       (check-match-failure (matcher big-syntax (hasheq) (vector)))
       (check-match-failure (matcher wrong-syntax (hasheq) (vector))))
     
     ;check matching against nested fixed lists
     (let* ([syntax '((invoke return value) literal (a b))]
            [id1 'x]
            [id2 'y]
            [id3 'z]
            [pattern (fixed-list '() 
                                 (list (pattern-identifier id1 0) 
                                       (literal-identifier 'literal)
                                       (fixed-list '() (list (pattern-identifier id2 1)
                                                             (pattern-identifier id3 2)))))]
            [register (hasheq id1 0 id2 1 id3 2)]
            [reg-size 3]
            [pvec (make-vector reg-size (void))])
       (define result ((make-matcher pattern (hasheq) register reg-size) syntax (hasheq) pvec))
       (check-equal? pvec (vector '(invoke return value) 'a 'b)))
     
     ;This test is flawed and needs to be changed or removed. 
     ;datum should not be used to match symbols. 
     (let* ([syntax '(a b c (d e (f g h) i) j k)]
            [id1 'x]
            [id2 'y]
            [pattern (improper-list 
                      '() 
                      (list (datum 'a) (datum 'b) (pattern-identifier id1 0))
                      (fixed-list 
                       '()
                       (list
                        (fixed-list
                         '()
                         (list (datum 'd) 
                               (datum 'e)
                               (pattern-identifier id2 1)
                               (datum 'i)))
                        (datum 'j)
                        (datum 'k))))]
            [register (hasheq id1 0 id2 1)]
            [pvec (make-vector 2 (void))]
            [matcher (make-matcher pattern (hasheq) register 2)]
            [too-short '(a b)]
            [too-long (cons 'z syntax)])
       (define result (matcher syntax (hasheq) pvec))
       (check-match-success result)
       (check-equal? pvec (vector 'c '(f g h)))
       (check-match-failure (matcher too-short (hasheq) pvec))
       (check-match-failure (matcher too-long (hasheq) pvec))
       (check-match-failure (matcher 'weird-syntax (hasheq) pvec)))
     
     ;test matching of ellipses list against lists of varying length
     (let* ([syntax '(a b c d e f g)]
            [id 'x]
            [id2 'y]
            [pattern (ellipses-list '() 
                                    (list (datum 'a) (datum 'b) (pattern-identifier id2 1)) 
                                    (pattern-identifier id 0))]
            [register (hasheq id 0 id2 1)]
            [pvec (make-vector 2 (void))]
            [too-small '(a b c)]
            [really-small '(a)]
            [just-right '(a b c d)]
            [_matcher (make-matcher pattern (hasheq) register 2)]
            [matcher (λ (s) 
                       (define match-result (_matcher s (hasheq) pvec))
                       (if (pattern-mismatch? match-result) match-result pvec))])
       (check-equal? (matcher syntax) (vector '(d e f g) 'c))
       (check-equal? (matcher just-right) (vector '(d) 'c))
       (check-match-failure (matcher too-small))
       (check-match-failure (matcher really-small))
       (check-match-failure (matcher 5)))
     
     ;test matching of a ((x ...) ...) against a nested list
     (let* ([syntax '((a b c) (c d e) (f g h) (i j k) (l m) (n))]
            [pattern (ellipses-list '() (list) (ellipses-list '() (list) (pattern-identifier 'x 0)))]
            [pvec (vector (void))])
       (define match-result ((make-matcher pattern (hasheq) (hasheq 'x 0) 1) syntax (hasheq) pvec))
       (check-match-success match-result)
       (check-equal? pvec  (vector syntax)))
     
     ;test parsing of an ellipses list and resulting matcher.
     (let*-values
         ([(pattern indexes size) (parse-transformer-pattern '(test1 test2 ...) (set))]
          [(ids) (compute-ellipses-nesting pattern size)])
       (check-pred ellipses-list? pattern)
       (check-equal? (ellipses-list-sub-patterns pattern) (list (pattern-identifier 'test1 0)))
       (check-equal? (ellipses-list-tail-pattern pattern) (pattern-identifier 'test2 1))
       (check-equal? ids (vector 0 1)))
     
     ;test parsing of 3-level deep ellipses pattern and resulting matcher
     (let*-values
         ([(pattern register size) 
           (parse-transformer-pattern 
            '(((((a ...) (b ...) (c ...)) ...) -) ...) (set))]
          [(ids) (compute-ellipses-nesting pattern size)])
       (check-equal? 
        pattern
        (ellipses-list
         '(((((a ...) (b ...) (c ...)) ...) -) ...) '()
         (fixed-list 
          '((((a ...) (b ...) (c ...)) ...) -)
          (list
           (ellipses-list
            '(((a ...) (b ...) (c ...)) ...) '()
            (fixed-list
             '((a ...) (b ...) (c ...))
             (list
              (ellipses-list
               '(a ...) '()
               (pattern-identifier 'a 0))
              (ellipses-list
               '(b ...) '()
               (pattern-identifier 'b 1))
              (ellipses-list
               '(c ...) '()
               (pattern-identifier 'c 2)))))
           (pattern-identifier '- 3)))) )              
       (check-equal? ids (vector 3 3 3 1)))
     
     ;checks that duplicate ids lead to syntax error
     #;(let-values ([(pattern register size)
                   (parse-transformer-pattern 
                    '((((a ...) (b ...)) ...) (d e f g . ((h (i j (k l m (a)))) ...))) (set))])
       (check-exn syntax-error? (lambda () (compute-ellipses-nesting pattern)))
       )
     (check-exn syntax-error? (lambda () 
                                (parse-transformer-pattern 
                                 '((((a ...) (b ...)) ...) (d e f g . ((h (i j (k l m (a)))) ...))) (set))))
     
     ;basic ellipses nesting test
     (let*-values 
         ([(pattern register size)
             (parse-transformer-pattern
              '(a b . "bob") (set 'd 'e))]
            [(ids) (compute-ellipses-nesting pattern size)])
       (check-equal?
        pattern
        (improper-list
         '(a b . "bob")
         (list
          (pattern-identifier 'a 0)
          (pattern-identifier 'b 1))
         (datum "bob")))
       (check-equal? ids (vector 0 0)))
     
     
     ;test parsing of ellipses template
     (let*-values
         ([(template _ __)
           (parse-transformer-template
            '((a ...)) (vector 1) (hasheq 'a 0))])
       (check-equal?
        template
        (template-list 
         '((a ...))
         (list 
          (template-list
           '(a ...)
           (list
            (ellipses-template
             '(a ...)
             (template-pattern-identifier 'a 1 0)
             1
             0
             (hasheq '0 (set 1))
             (set 0))))))))
     
     ;test improper-template-list parsing
     ;commented this flawed test out for now. You can't make an improper list if the last value is a list!
     ; It should be revisite using
     ; nested vectors when vectors become supported.
     #;(let* ([template 
               (parse-transformer-template
                '(a ... ... #f "c" #\5 6 . ((very nested) . lists))
                (hasheq 'a 2) (hasheq 'a 0))])
         (check-equal?
          template
          (improper-template-list 
           '(a ... ... #f "c" #\5 6 . ((very nested) . lists))
           (list
            (ellipses-template
             'a ;TODO syntax should probably include the ellipses too
             (template-pattern-identifier 'a 0)
             2
             (set 'a))
            (template-datum #f)
            (template-datum "c")
            (template-datum #\5)
            (template-datum 6))
           (improper-template-list 
            '((very nested) . lists)
            (list
             (template-list
              '(very nested)
              (list
               (template-literal-identifier 'very 0)
               (template-literal-identifier 'nested 1))))
            (template-literal-identifier 'lists 2)))))
     
     ;test combination of an ellipses matcher and template to match syntax and then output it
     ;tests when an identifier appears in multiple positions in a template with different ellipses nestings
     ;tests when the ellipses nesting between matcher and template don't match
     (let*-values 
         ([(pattern register size)
           (parse-transformer-pattern
            '((a ...) b) (set))]
          [(pattern-nesting) 
           (compute-ellipses-nesting pattern size)]
          [(parse) (λ (t) (define-values (result _ __) (parse-transformer-template t pattern-nesting register)) result)]
          [(template1)
           (parse
            '(a ...))]
          [(template2)
           (parse
            '(b c d f))]
          [(template3) 
           (parse
            '(g (f 'b (h a ...) "a" b)))]
          [(template4)
           (parse
            '((a b) ...))]
          [(bad-syntax1)
           '(a (a ...) b)])
       #;(for ([template (list template1 template2 template3 template4)])
           (verify-template-ellipses-nesting template pattern-nesting))
       (for ([template (list bad-syntax1)])
         (check-exn syntax-error? 
                    (lambda () 
                      (parse template)))))
     
     ;is this two tests in one?
     ;the first test checks that an error is thrown if there is no identifier in an ellipses template
     ;the second, tests that we can find an identifier deep inside of an ellipses list
     ;TODO: nest inside of vectors once they are supported
     (let* ([bad-syntax
             '(5 ...)]
            [really-nested-identifier-syntax
             '((1 2 3 . (4 5 . a)) ...)])
       (check-exn syntax-error? (lambda () (parse-transformer-template bad-syntax (vector 1) (hasheq 'a 0))))
       (parse-transformer-template really-nested-identifier-syntax (vector 1) (hasheq 'a 0))
       #t)
     
     
     ;tests that verify-template-ellipses-nesting works correctly with 
     ; varying ways of nesting an identifier in multiple ellipses in a template
     (let*-values
         ([(pattern register size)
           (parse-transformer-pattern
            '((((a ...) ...)) ...) (set))]
          [(nesting) (compute-ellipses-nesting pattern size)]
          [(parse) (λ (t) (define-values (result _ __) (parse-transformer-template t nesting register)) result)]
          [(good1)
           (parse
            '((a ... ...) ...))]
          [(good2)
           (parse
            '(a ... ... ...))]
          [(good3)
           (parse
            '((a ...) ... ...))]
          [(bad1)
           '((a ... ... ...) ...)]
          [(bad2)
           '((a ... ...) ... ...)])
       #;(for ([t (list good1 good2 good3)])
           (verify-template-ellipses-nesting t nesting))
       (for ([t (list bad1 bad2)])
         (check-exn syntax-error? 
                    (lambda () 
                      (parse t)))))
     
     ;test flatten#
     (let* ([3-level 
             '(
               ((a b c) (d e f)) 
               ((g h i) (j k l)) 
               ((m n o) (p q r) (s t u)))])
       (check-equal?
        (flatten# 3-level 1)
        '((a b c) (d e f) (g h i) (j k l) (m n o) (p q r) (s t u)))
       (check-equal?
        (flatten# 3-level 2)
        '(a b c d e f g h i j k l m n o p q r s t u)))
     
     ;more complex flatten# test
     (let ([4-level
            '(
              (
               (
                (a b) (c d))
               (
                (e f) (g h))
               (
                (i j) (k l) (m n))
               (
                (o p q r)))
              (
               (
                (s t))
               (
                (u v) (w x) (y z 1 2 3))))])
       (check-equal? (flatten# 4-level 0) 4-level)
       (check-equal?
        (flatten# 4-level 1)
        '(
          (
           (a b) (c d))
          (
           (e f) (g h))
          (
           (i j) (k l) (m n))
          (
           (o p q r))
          (
           (s t))
          (
           (u v) (w x) (y z 1 2 3))))
       (check-equal?
        (flatten# 4-level 2)
        '(
          (a b) (c d) (e f) (g h)
                (i j) (k l) (m n) 
                (o p q r) (s t) (u v) 
                (w x) (y z 1 2 3)))
       (check-equal? 
        (flatten# 4-level 3)
        '(a b c d e f g h i j k l m n o p q r s t u v w x y z 1 2 3))
       #;(check-equal? 
        (flatten# 4-level 4)
        '(a b c d e f g h i j k l m n o p q r s t u v w x y z 1 2 3))
       #;(check-exn exn:fail:contract?
                  (lambda () (flatten# 4-level 4))))
     
     ;tests find-regular-ids, make-rewriter. 
     ;Basically a template is created and used to make a macro.
     ;That macro is then invoked and its resulting syntax is checked.
     (let* 
         ([pattern-nestings (vector 1 1 1 2)]
          [pattern-indeces (hasheq 'a 0 'b 1 'c 2 'd 3)]
          [template
           (let ()
           (define-values (result reg-reg reg-reg-size) 
             (parse-transformer-template
              '((a ...) ((b c (d ...)) ...) . (e f g ('h "i" (#\k)) 'l))
              pattern-nestings
              pattern-indeces))
             result)]
          [reg-ids (find-regular-ids template)]
          [rewriter (make-rewriter template pattern-nestings pattern-indeces)])
       (define pvec 
         (vector (list 1 2 3) (list 4 5 6) (list 7 8 9) '((10 11 12) (13 14 15) (16 17 18))))
       (define rvec (vector 'e.1 'f.1 'g.1 'quote.1 'h.1 'l.1))
       (define tp-vec (make-vector 4))
       (cfor (i 0 (< i 4) (add1 i))
             (vector-set! tp-vec i (make-vector (add1 (vector-ref pattern-nestings i))))
             (vector-set! (vector-ref tp-vec i) (vector-ref pattern-nestings i) (vector-ref pvec i)))
       (check-equal? 
        reg-ids
        (set 'e 'f 'g 'quote 'h 'l))
       (check-equal?
        (rewriter tp-vec rvec)
        '((1 2 3) ((4 7 (10 11 12)) (5 8 (13 14 15)) (6 9 (16 17 18))) . 
                  (e.1 f.1 g.1 ((quote.1 h.1) "i" (#\k)) (quote.1 l.1)))))
     
          ;simple test of the 'and' macro definition in the presence of local defines. 
     ;This would actually cause the and macro to not work as expected if used with let-syntax, but that is beyond this test.
     (let*-values 
         ([(rules) (parse-syntax-rules 
                    and-syntax (hasheq))]    
          [(macro) (make-macro-transformer rules)]
          [(env) (hasheq 'and (cons 'and 'and) 'if (cons 'if 'if))]
          [(r1 e1) (macro '(and) env)]
          [(r2 e2) (macro '(and "bob") env)]
          [(r3 e3) (macro '(and #f #t) env)]
          #;[(qe3-rev) (invert-hash qe3)])
       (check-equal? #t r1)
       (check-equal? "bob" r2)
       (check-not-exn
        (lambda ()
          (match r3
            [(list (? (sym-matcher 'if)) #f (list (? (sym-matcher 'and)) #t) #f)
             #t])))
       #;(check-equal? qe1 qenv)
       #;(check-equal? qe2 qenv)
       #;(check-quote-env qe3 '(if and)))
     
     ;with no local bindings for and and if, a macro should generate identifiers whose binding denotes the same top-level value as their original identifiers.
     #;(let*-values
         ([(macro) (parse-syntax-transformer and-syntax (hasheq))]
          [(s e) (macro '(and a b 1) (hasheq))]
          [(req) (invert-hash qe)]
          [(new-and new-if) (values (hash-ref req 'and) (hash-ref req 'if))])
       (check-equal? (length (hash-keys req)) 2)
       (check-equal? (hash-ref e new-and) (denotation 'and))
       (check-equal? (hash-ref e new-if) (denotation 'if)))
     
     ;test to show that we are handling quote forms properly by not treating them specially, since we don't know the exact binding of each quote identifier yet
     (let*-values
         ([(rules) (parse-syntax-rules
                    '(syntax-rules ()
                       [(who-cares a) (list a 'a)]) (hasheq))]
          [(macro) (make-macro-transformer rules)]
          [(env) (hasheq)]
          [(r1 e1) (macro '(imagine-this-keyword-is-bound-to-the-macro (list 1 2 3)) (hasheq))])
       (check-not-exn
        (lambda ()
          (match r1
            [(list (? (sym-matcher 'list)) (list 'list 1 2 3) (list (? (sym-matcher 'quote)) (list 'list 1 2 3))) #t])))
       #;(check-quote-env qe1 '(quote list)))
     
     ;TODO add test for edge case of syntax-rules with empty rule list
     ;we should be handling this correctly but an automated test would make us more certain
     )))
