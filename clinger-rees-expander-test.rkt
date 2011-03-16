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

(let ([pattern (pattern-identifier 'v)]
      [s1 'v]
      [s2 '(a b c d)]
      [s3 #(bob dole is cool)]
      [s4 #hash( (1 . 2) (3 . 4) (5 . 6))])
  (for ([s (list s1 s2 s3 s4)])
    (define result (match-input pattern s (hash) (hash)))
    (check-match-success result)
    (check-equal? result (hash (input-pattern-source pattern) s))))

(let* ([id 'x]
       [pattern (literal-identifier id)]
       [bad-syntaxes (list 'a "b" #\x `(,id) #(x))]
       [empty-env (hash)]
       [env1 (hash id (gensym id))]
       [env2 (hash id (gensym id))]
       [id2 'y]
       [diff-env (hash id2 (hash-ref env1 id))])
  (for ([env (list empty-env env1 env2)])
    (define result (match-input pattern id env env))
    (check-match-success result)
    (check-pred hash-empty? result))
  (for ([s bad-syntaxes])
    (check-match-failure (match-input pattern s empty-env empty-env)))
  (for ([e1 (list empty-env env1 env2)]
        [e2 (list env1 env2 empty-env)])
    (check-match-failure (match-input pattern id e1 e2)))
  (check-match-success (match-input pattern id2 env1 diff-env))
  (check-match-failure (match-input pattern id env1 diff-env))
  (check-match-failure (match-input pattern id2 env2 diff-env)))

(let* ([syntax '(a b c d e f g)]
       [big-syntax (append syntax '(h))]
       [small-syntax (take syntax (sub1 (length syntax)))]
       [wrong-syntax (map (lambda (s) (if (eqv? s 'd) 'z s)) syntax)]
       [pattern (fixed-list syntax (map (lambda (s) (datum s)) syntax))])
  (define good-result (match-input pattern syntax (hash) (hash)))
  (check-match-success good-result)
  (check-pred hash-empty? good-result)
  (check-match-failure (match-input pattern big-syntax (hash) (hash)))
  (check-match-failure (match-input pattern wrong-syntax (hash) (hash))))

(let* ([syntax '((invoke return value) literal (a b))]
       [id1 'x]
       [id2 'y]
       [id3 'z]
       [pattern (fixed-list '() 
                            (list (pattern-identifier id1) 
                                  (literal-identifier 'literal)
                                  (fixed-list '() (list (pattern-identifier id2)
                                                        (pattern-identifier id3)))))])
  (define result (match-input pattern syntax (hash) (hash)))
  (check-equal? result (hash id1 '(invoke return value) id2 'a id3 'b)))

;This test is flawed and needs to be fixed. 
;datum should not be used to match symbols. 
;They should be used to match lists of the form (quote symbol)
(let* ([syntax '(a b c (d e (f g h) i) j k)]
       [id1 'x]
       [id2 'y]
      [pattern (improper-list 
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
  (define result (match-input pattern syntax (hash) (hash)))
  (check-equal? result (hash id1 'c id2 '(f g h)))
  (check-match-failure (match-input pattern too-short (hash) (hash)))
  (check-match-failure (match-input pattern too-long (hash) (hash)))
  (check-match-failure (match-input pattern 'weird-syntax (hash) (hash))))

(let* ([syntax '(a b c d e f g)]
       [id 'x]
       [id2 'y]
       [pattern (ellipses-list '() 
                               (list (datum 'a) (datum 'b) (pattern-identifier id2)) 
                               (pattern-identifier id))]
       [too-small '(a b c)]
       [really-small '(a)]
       [just-right '(a b c d)])
  (check-equal? (match-input pattern syntax (hash) (hash)) (hash id '(d e f g) id2 'c))
  (check-equal? (match-input pattern just-right (hash) (hash)) (hash id '(d) id2 'c))
  (check-match-failure (match-input pattern too-small (hash) (hash)))
  (check-match-failure (match-input pattern really-small (hash) (hash)))
  (check-match-failure (match-input pattern 5 (hash) (hash))))

(let* ([syntax '((a b c) (c d e) (f g h) (i j k) (l m) (n))]
       [pattern (ellipses-list '() (list) (ellipses-list '() (list) (pattern-identifier 'x)))])
  (check-equal? (match-input pattern syntax (hash) (hash)) (hash 'x syntax)))

(let*
    ([pattern (parse-transformer-pattern '(test1 test2 ...) (set))]
     [ids (compute-ellipses-nesting pattern)])
  (check-pred ellipses-list? pattern)
  (check-equal? (ellipses-list-sub-patterns pattern) (list (pattern-identifier 'test1)))
  (check-equal? (ellipses-list-tail-pattern pattern) (pattern-identifier 'test2))
  (check-equal? ids (hash 'test1 0 'test2 1)))

(let*
    ([pattern 
      (parse-transformer-pattern 
       '(((((a ...) (b ...) (c ...)) ...) -) ...) (set))]
     [ids (compute-ellipses-nesting pattern)])
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
          (pattern-identifier 'a))
         (ellipses-list
          '(b ...) '()
          (pattern-identifier 'b))
         (ellipses-list
          '(c ...) '()
          (pattern-identifier 'c)))))
      (pattern-identifier '-)))) )              
  (check-equal? ids (hash 'a 3 'b 3 'c 3 '- 1)))

(let ([pattern 
       (parse-transformer-pattern 
        '((((a ...) (b ...)) ...) (d e f g . ((h (i j (k l m (a)))) ...))) (set))])
  (check-exn syntax-error? (lambda () (compute-ellipses-nesting pattern)))
  )

(let* ([pattern
        (parse-transformer-pattern
         '(a b |.| (d #t 5 "bob" #\c e)) (set 'd 'e))]
       [ids (compute-ellipses-nesting pattern)])
  (check-equal?
   pattern
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

(let*
    ([template
      (parse-transformer-template
       '((a ...))
       (set 'a) (hash))])
  (check-equal?
   template
   (template-list 
    '((a ...))
    (list 
     (template-list
      '(a ...)
      (list
       (ellipses-template
        'a
        (template-identifier 'a)
        1
        (set 'a))))))))

(let* ([template 
        (parse-transformer-template
         '(a ... ... #f "c" #\5 6 |.| ((very nested) |.| lists))
         (set 'a) (hash))])
  (check-equal?
   template
   (improper-template-list 
    '(a ... ... #f "c" #\5 6 |.| ((very nested) |.| lists))
    (list
     (ellipses-template
      'a ;TODO syntax should probably include the ellipses too
      (template-identifier 'a)
      2
      (set 'a))
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

(let* ([pattern
        (parse-transformer-pattern
         '((a ...) b) (set))]
       [pattern-nesting 
        (compute-ellipses-nesting pattern)]
       [pattern-ids (apply set (hash-keys pattern-nesting))]
       [template1
        (parse-transformer-template
         '(a ...) pattern-ids (hash))]
       [template2
        (parse-transformer-template
         '(b c d f) pattern-ids (hash))]
       [template3 
        (parse-transformer-template
         '(g (f 'a (h a ...) "a" b)) pattern-ids (hash))]
       [bad-template1
        (parse-transformer-template
         '((a b) ...) pattern-ids (hash))]
       [bad-template2
        (parse-transformer-template
        '(a (a ...) b) pattern-ids (hash))])
  #t
  (for ([template (list template1 template2 template3)])
    (verify-template-ellipses-nesting template pattern-nesting))
  (for ([template (list bad-template1 bad-template2)])
    (check-exn syntax-error? 
               (lambda () 
                 (verify-template-ellipses-nesting template pattern-nesting)))))

(let* ([bad-syntax
        '(5 ...)]
       [really-nested-identifier-syntax
        '((1 2 3 |.| (4 5 |.| ('c "d" a #\g))) ...)])
  (check-exn syntax-error? (lambda () (parse-transformer-template bad-syntax (set 'a) (hash))))
  (parse-transformer-template really-nested-identifier-syntax (set 'a) (hash))
  #t)

(let* ([pattern
         (parse-transformer-pattern
          '((((a ...) ...)) ...) (set))]
        [nesting (compute-ellipses-nesting pattern)]
        [good1
          (parse-transformer-template
           '((a ... ...) ...)
           (set 'a) (hash))]
        [good2
         (parse-transformer-template
          '(a ... ... ...)
          (set 'a) (hash))]
        [good3
         (parse-transformer-template
          '((a ...) ... ...)
          (set 'a) (hash))]
        [bad1
         (parse-transformer-template
          '((a ... ... ...) ...)
          (set 'a) (hash))]
        [bad2
         (parse-transformer-template
          '((a ... ...) ... ...)
          (set 'a) (hash))])
   (for ([t (list good1 good2 good3)])
     (verify-template-ellipses-nesting t nesting))
   (for ([t (list bad1 bad2)])
     (check-exn syntax-error? 
                (lambda () 
                  (verify-template-ellipses-nesting t nesting)))))

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
  (check-exn exn:fail:contract?
               (lambda () (flatten# 4-level 4))))
                  
(let* 
    ([pattern-ids (set 'a 'b 'c 'd)]
     [template
      (parse-transformer-template
       '((a ...) ((b c (d ...)) ...) . (e f g ('h "i" (#\k)) 'l))
       pattern-ids (hash))]
     [reg-ids (find-regular-identifiers template pattern-ids)]
     [rewriter (make-rewriter template)]
     [sub-map 
      (hash 
       'a (list 1 2 3)
       'b (list 4 5 6)
       'c (list 7 8 9)
       'd '((10 11 12) (13 14 15) (16 17 18))
       'e 'e.1
       'f 'f.1
       'g 'g.1)])
  (check-equal? 
   reg-ids
   (set 'e 'f 'g))
  (check-equal?
   (rewriter sub-map)
   '((1 2 3) ((4 7 (10 11 12)) (5 8 (13 14 15)) (6 9 (16 17 18))) . 
             (e.1 f.1 g.1 (h "i" (#\k)) l))))