#lang racket
(require racket rackunit
         "clinger-rees-syntax-rules.rkt"
         "clinger-rees-parser.rkt")

;test that let-syntax returns the expected environment and body syntax
;TODO test that the parsed macros actually work properly
(let*-values
    ([(body-syntax)
      '(when #t
         (unless #f
           5))]
     [(body-syntax-returned extended-env)
      (reduce-let-syntax
       `(((when (syntax-rules ()
              [(when test stmt1 stmt2 ...) 
               (if test
                   (begin stmt1 stmt2 ...) (void))]))
         (unless (syntax-rules ()
                   [(unless test stmt1 stmt2 ...)
                    (if (not test)
                        (begin stmt1 stmt2 ...) (void))])))
       ,body-syntax) 
       (hash))])
  (check-equal? (set 'when 'unless) (apply set (hash-keys extended-env)))
  (check-equal? body-syntax body-syntax-returned))

;test definition of 'and' macro with letrec
(let*-values
    ([(and-macro-syntax)
      '(syntax-rules ()
         ((my-and) #t)
         ((my-and test) test)
         ((my-and test1 test2 ...)
          (if test1 (my-and test2 ...) #f)))]
     [(body-syntax)
      '(my-and 1 2 3 4)]
     [(body-syntax-returned env)
      (reduce-letrec-syntax
       `(((my-and ,and-macro-syntax))
         ,body-syntax) (hash 'if 'if))]
     [(macro) (hash-ref env 'my-and)]
     [(s1 e1 qe1) (macro body-syntax-returned env (hash))])
  (check-equal? (set 'my-and 'if) (apply set (hash-keys env)))
  ;TODO convert these print statements into actual assertions. 
  ;The print statements look like they are printing out the right things for now.
  (printf "~a\n" s1)
  (printf "~a\n" e1))



                   
    
