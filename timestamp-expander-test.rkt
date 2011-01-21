#lang scheme
(require racket rackunit
         "timestamp-expander.rkt")

(let* ([syntax 
       `(,(ts-syntax 'let 0) ([a 1] [b 2] [c (+ 3 4)])
          (+ a b c))]
       [expected
        `((,(ts-syntax 'lambda 0) (a b c) (+ a b c)) 1 2 (+ 3 4))])
  (check-equal? (rewrite-let-as-lambda syntax) expected))

(let* ([syntax
        `(,(ts-syntax 'letrec 0) ((even?
                   (lambda (n)
                     (if (zero? n)
                         #t
                         (odd? (- n 1)))))
                  (odd?
                   (lambda (n)
                     (if (zero? n)
                         #f
                         (even? (- n 1))))))
           (even? 88))])
  (check-not-exn 
   (lambda () 
     (match (rewrite-letrec-as-lambda syntax) 
       [(list (list (ts-syntax 'lambda 0) (list 'even? 'odd?) 
                    (list (list (ts-syntax 'lambda 0) (list tmp1 tmp2) 
                                (list 'set! 'even? tmp1) 
                                (list 'set! 'odd? tmp2) 
                                (list 'even? 88)) 
                          (list 'lambda (list 'n) (list 'if (list 'zero? 'n) #t (list 'odd? (list '- 'n 1)))) 
                          (list 'lambda (list 'n) (list 'if (list 'zero? 'n) #f (list 'even? (list '- 'n '1)))))) #f #f) 
        #t]))))

(let ([syntax 
       `(,(ts-syntax 'let* 0) ([x 1] [y (+ x 1)]) (+ x y))])
  (check-equal? (rewrite-let*-as-lambda syntax) 
                 `((,(ts-syntax 'lambda 0) (x) ((,(ts-syntax 'lambda 0) (y) (begin (+ x y))) (+ x 1))) 1)))

(check-equal? (alpha-rename (parser-state 'a (hash))) 'a)
(check-equal? (macro-expand 'a) 'a)
(macro-expand '(or 1 2 3))
(macro-expand '(let ([v #t]) (or #f v)))

