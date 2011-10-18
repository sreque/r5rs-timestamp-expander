(module clinger-rees-env racket
  (provide r5rs-top-level-env)
  (require racket
           "clinger-rees-syntax-rules.rkt")
  ;our goal is to define all of the symbols needed to handle r5rs code
  
  ;Symbols for all top-level variables
  (define top-level-variables
    '(* + - / < <= = > >= abs acos angle append apply asin assoc assq assv atan boolean? caar cadr call-with-current-continuation
        call-with-input-file call-with-output-file call-with-values car cdddar cddddr cdr ceiling char->integer char-alphabetic? 
        char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? char-ready? char-upcase 
        char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port complex? 
        cons cos current-input-port current-output-port denominator display dynamic-wind eof-object? eq? equal? eqv? eval even? 
        exact->inexact exact? exp expt floor for-each force gcd imag-part inexact->exact inexact? input-port? integer->char 
        integer? interaction-environment lcm length list list->string list->vector list-ref list-tail list? load log magnitude
        make-polar make-rectangular make-string make-vector map max member memq memv min modulo negative? newline not 
        null-environment null? number->string number? numerator odd? open-input-file open-output-file output-port? pair? 
        peek-char positive? procedure? quotient rational? rationalize read read-char real-part real? remainder reverse round 
        scheme-report-environment set! set-car! set-cdr! sin sqrt string string->list string->number string->symbol string-append 
        string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-copy string-fill! string-length string-ref 
        string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol? tan transcript-off 
        transcript-on truncate values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? 
        with-input-from-file with-output-to-file write write-char zero? if))
  
  (define r5rs-top-level-env 
    (for/hash ([s top-level-variables])
      (values s s)))
  
  (define (parse-internal-macro syntax)
    (parse-syntax-transformer syntax (hash)))
  
  (define-syntax (define-macro stx)
    (define subs (syntax->list stx))
    (define id (syntax->datum (cadr subs)))
    (define new-id (string->symbol (format "~a-macro" (symbol->string id))))
    (define value (syntax->datum (caddr subs)))
    #;(printf "id=~a new-id=~a\nvalue=~a\n" id new-id value)
    (datum->syntax
     stx
     `(begin 
        (define 
          ,new-id
          (parse-internal-macro (quote ,value)))
        (set! r5rs-top-level-env (hash-set r5rs-top-level-env (quote ,id) ,new-id)))))
  
  
  (define-macro letrec
    (syntax-rules () 
      [(_ ((var init) ...) . body) 
       (let ((var 'undefined) ...)  ;should I use (void) here?
         (let ((var (let ((temp init)) (lambda () (set! var temp)))) 
               ... 
               (bod (lambda () . body))) 
           (var) ... (bod)))]))
  
  (define-macro cond
    (syntax-rules (else =>)
      ((cond (else result1 result2 ...))
       (begin result1 result2 ...))
      ((cond (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (cond clause1 clause2 ...))))
      ((cond (test)) test)
      ((cond (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)))))
  
  
  (define-macro case
    (syntax-rules (else)
      ((case (key ...)
         clauses ...)
       (let ((atom-key (key ...)))
         (case atom-key clauses ...)))
      ((case key
         (else result1 result2 ...))
       (begin result1 result2 ...))
      ((case key
         ((atoms ...) result1 result2 ...))
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)))
      ((case key
         ((atoms ...) result1 result2 ...)
         clause clauses ...)
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)
           (case key clause clauses ...)))))
  
  
  (define-macro and
    (syntax-rules ()
      ((and) #t)
      ((and test) test)
      ((and test1 test2 ...)
       (if test1 (and test2 ...) #f))))
  
  
  (define-macro or
    (syntax-rules ()
      ((or) #f)
      ((or test) test)
      ((or test1 test2 ...)
       (let ((x test1))
         (if x x (or test2 ...))))))
  
  
  (define-macro let
    (syntax-rules ()
      ((let ((name val) ...) body1 body2 ...)
       ((lambda (name ...) body1 body2 ...)
        val ...))
      ((let tag ((name val) ...) body1 body2 ...)
       ((letrec ((tag (lambda (name ...)
                        body1 body2 ...)))
          tag)
        val ...))))
  
  
  (define-macro let*
    (syntax-rules ()
      ((let* () body1 body2 ...)
       (let () body1 body2 ...))
      ((let* ((name1 val1) (name2 val2) ...)
         body1 body2 ...)
       (let ((name1 val1))
         (let* ((name2 val2) ...)
           body1 body2 ...)))))
  
  (define-macro do
    (syntax-rules ()
      ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
       (letrec
           ((loop
             (lambda (var ...)
               (if test
                   (begin
                     (if #f #f)
                     expr ...)
                   (begin
                     command
                     ...
                     (loop (do "step" var step ...)
                           ...))))))
         (loop init ...)))
      ((do "step" x)
       x)
      ((do "step" x y)
       y)))
  
;this was taken from http://www.rhinocerus.net/forum/lang-scheme/98742-quasiquote-syntax-rules-macro.html
  (define-macro quasiquote
    (syntax-rules
        (quasiquote unquote unquote-splicing quote)
      ((quasiquote a)
       (quasiquote "zero" a))
      ((quasiquote l (quote a))
       (list 'quote (quasiquote l a)))
      ((quasiquote "zero" (unquote a))
       a)
      ((quasiquote "zero" b . (unquote a))
       (cons b a))
      ((quasiquote ("succ" . l) (unquote a))
       (list 'unquote (quasiquote l a)))
      ((quasiquote l (unquote-splicing a))
       (syntax-error "unquote-splicing"))
      ((quasiquote "zero" ((unquote-splicing a) . r))
       (append a (quasiquote "zero" r)))
      ((quasiquote ("succ" . l) ((unquote-splicing a) . r))
       (cons (list 'unquote-splicing (quasiquote l a))
             (quasiquote ("succ" . l) r)))
      ((quasiquote l (quasiquote a))
       (list 'quasiquote (quasiquote ("succ" . l) a)))
      ((quasiquote l (r . r1))
       (cons (quasiquote l r) (quasiquote l r1)))
      #;((quasiquote l #(r ...))
       (list->vector (quasiquote l (r ...))))
      ((quasiquote l a)
       (quote a))))


  #;(display r5rs-top-level-env)
  )
