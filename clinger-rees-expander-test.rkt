#lang racket
(require racket rackunit
         "clinger-rees-expander.rkt")
(define (check-match-success result)
  (check-pred hash? result))

(define (check-match-failure result)
  (pattern-mismatch? result))

(define hash-empty? (compose not hash-iterate-first))

(let* ([syntax 'a]
      [m1 (datum 'a)]
      [m2 (datum "a")]
      [m3 (datum #\a)]
      [m4 (fixed-list '() (list (datum syntax)))])
  (define result (match-input m1 syntax (hash) (hash)))
  (check-match-success result)
  (check-pred hash-empty? result (format "hash should be empty: ~a\n" result))
  (for ([m (list m2 m3 m4)])
    (check-match-failure (match-input m syntax (hash) (hash)))))

(let ([matcher (pattern-identifier 'v)]
      [s1 'v]
      [s2 '(a b c d)]
      [s3 #(bob dole is cool)]
      [s4 #hash( (1 . 2) (3 . 4) (5 . 6))])
  (for ([s (list s1 s2 s3 s4)])
    (define result (match-input matcher s (hash) (hash)))
    (check-match-success result)
    (check-equal? result (hash (input-matcher-source matcher) s))))

(let* ([id 'x]
       [matcher (literal-identifier id)]
       [bad-syntaxes (list 'a "b" #\x `(,id) #(x))]
       [empty-env (hash)]
       [env1 (hash id (gensym id))]
       [env2 (hash id (gensym id))]
       [id2 'y]
       [diff-env (hash id2 (hash-ref env1 id))])
  (for ([env (list empty-env env1 env2)])
    (define result (match-input matcher id env env))
    (check-match-success result)
    (check-pred hash-empty? result))
  (for ([s bad-syntaxes])
    (check-match-failure (match-input matcher s empty-env empty-env)))
  (for ([e1 (list empty-env env1 env2)]
        [e2 (list env1 env2 empty-env)])
    (check-match-failure (match-input matcher id e1 e2)))
  (check-match-success (match-input matcher id2 env1 diff-env))
  (check-match-failure (match-input matcher id env1 diff-env))
  (check-match-failure (match-input matcher id2 env2 diff-env)))

(let* ([syntax '(a b c d e f g)]
       [big-syntax (append syntax '(h))]
       [small-syntax (take syntax (sub1 (length syntax)))]
       [wrong-syntax (map (lambda (s) (if (eqv? s 'd) 'z s)) syntax)]
       [matcher (fixed-list syntax (map (lambda (s) (datum s)) syntax))])
  (define good-result (match-input matcher syntax (hash) (hash)))
  (check-match-success good-result)
  (check-pred hash-empty? good-result)
  (check-match-failure (match-input matcher big-syntax (hash) (hash)))
  (check-match-failure (match-input matcher wrong-syntax (hash) (hash))))

(let* ([syntax '((invoke return value) literal (a b))]
       [id1 'x]
       [id2 'y]
       [id3 'z]
       [matcher (fixed-list '() 
                            (list (pattern-identifier id1) 
                                  (literal-identifier 'literal)
                                  (fixed-list '() (list (pattern-identifier id2)
                                                        (pattern-identifier id3)))))])
  (define result (match-input matcher syntax (hash) (hash)))
  (check-equal? result (hash id1 '(invoke return value) id2 'a id3 'b)))

(let* ([syntax '(a b c (d e (f g h) i) j k)]
       [id1 'x]
       [id2 'y]
      [matcher (improper-list 
                '() 
                (list (datum 'a) (datum 'b) (pattern-identifier id1))
                (fixed-list 
                 '()
                 (list
                  (fixed-list
                   '()
                   (list (datum 'd) 
                         (datum 'e)
                         (pattern-identifier id2)
                         (datum 'i)))
                  (datum 'j)
                  (datum 'k))))]
      [too-short '(a b)]
      [too-long (cons 'z syntax)])
  (define result (match-input matcher syntax (hash) (hash)))
  (check-equal? result (hash id1 'c id2 '(f g h)))
  (check-match-failure (match-input matcher too-short (hash) (hash)))
  (check-match-failure (match-input matcher too-long (hash) (hash)))
  (check-match-failure (match-input matcher 'weird-syntax (hash) (hash))))

(let* ([syntax '(a b c d e f g)]
       [id 'x]
       [id2 'y]
       [matcher (ellipses-list '() 
                               (list (datum 'a) (datum 'b) (pattern-identifier id2)) 
                               (pattern-identifier id))]
       [too-small '(a b c)]
       [really-small '(a)]
       [just-right '(a b c d)])
  (check-equal? (match-input matcher syntax (hash) (hash)) (hash id '(d e f g) id2 'c))
  (check-equal? (match-input matcher just-right (hash) (hash)) (hash id '(d) id2 'c))
  (check-match-failure (match-input matcher too-small (hash) (hash)))
  (check-match-failure (match-input matcher really-small (hash) (hash)))
  (check-match-failure (match-input matcher 5 (hash) (hash))))

(let* ([syntax '((a b c) (c d e) (f g h) (i j k) (l m) (n))]
       [matcher (ellipses-list '() (list) (ellipses-list '() (list) (pattern-identifier 'x)))])
  (check-equal? (match-input matcher syntax (hash) (hash)) (hash 'x syntax)))

(let*
    ([matcher (parse-transformer-pattern '(test1 test2 ...) (set))]
     [ids (compute-ellipses-nesting matcher)])
  (check-pred ellipses-list? matcher)
  (check-equal? (ellipses-list-sub-patterns matcher) (list (pattern-identifier 'test1)))
  (check-equal? (ellipses-list-tail-pattern matcher) (pattern-identifier 'test2))
  (check-equal? ids (hash 'test1 0 'test2 1)))

(let*
    ([matcher 
      (parse-transformer-pattern 
       '(((((a ...) (b ...) (c ...)) ...) -) ...) (set))]
     [ids (compute-ellipses-nesting matcher)])
  (check-equal? 
   matcher
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
          (pattern-identifier 'a))
         (ellipses-list
          '(b ...) '()
          (pattern-identifier 'b))
         (ellipses-list
          '(c ...) '()
          (pattern-identifier 'c)))))
      (pattern-identifier '-)))) )              
  (check-equal? ids (hash 'a 3 'b 3 'c 3 '- 1)))

(let ([matcher 
       (parse-transformer-pattern 
        '((((a ...) (b ...)) ...) (d e f g . ((h (i j (k l m (a)))) ...))) (set))])
  (check-exn syntax-error? (lambda () (compute-ellipses-nesting matcher)))
  )

(let* ([matcher
        (parse-transformer-pattern
         '(a b |.| (d #t 5 "bob" #\c e)) (set 'd 'e))]
       [ids (compute-ellipses-nesting matcher)])
  (check-equal?
   matcher
   (improper-list
    '(a b |.| (d #t 5 "bob" #\c e))
    (list
     (pattern-identifier 'a)
     (pattern-identifier 'b))
    (fixed-list
     '(d #t 5 "bob" #\c e)
     (list
      (literal-identifier 'd)
      (datum #t)
      (datum 5)
      (datum "bob")
      (datum #\c)
      (literal-identifier 'e)))))
  (check-equal? ids (hash 'a 0 'b 0)))

(let* ([template 
        (parse-transformer-template
         '(a ... ... #f "c" #\5 6 |.| ((very nested) |.| lists)))])
  (check-equal?
   template
   (improper-template-list 
    '(a ... ... #f "c" #\5 6 |.| ((very nested) |.| lists))
    (list
     (ellipses-template
      'a ;TODO syntax should probably include the ellipses too
      (template-identifier 'a)
      2)
     (template-datum #f)
     (template-datum "c")
     (template-datum #\5)
     (template-datum 6))
    (improper-template-list 
     '((very nested) |.| lists)
     (list
      (template-list
       '(very nested)
       (list
        (template-identifier 'very)
        (template-identifier 'nested))))
     (template-identifier 'lists)))))
     
     