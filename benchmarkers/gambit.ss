#!scheme-r5rs
(define-syntax when
  (syntax-rules ()
    ((when cond action) (if cond action #f))))

(define (file-directory? path)
  (eqv? (file-info-type (file-info path)) 'directory))

(define (foldl l xs init)
  (let loop ((accum init) (rest xs))
    (if (eqv? rest '())
      accum
      (loop (l accum (car rest)) (cdr rest)))))

(define (foldr l xs init)
  (let loop ((accum init) (rest xs))
    (if (eqv? rest '())
      accum
      (l (car rest) (loop accum (cdr rest))))))

(define (sub1 v) (- v 1))

(define (filter l ls)
  (foldr 
    (lambda (v a) (if (l v) (cons v a) a)) 
    ls
    '()))

(define (string-ends-with? str sfx)
  (define sfx-len (string-length sfx))
  (define str-len (string-length str))
  (if (< str-len sfx-len)
    #f
    (let loop ((idx (sub1 str-len)) 
               (sfx-idx (sub1 sfx-len)))
      (if (< sfx-idx 0)
        #t
        (if (not (char=? (string-ref str idx) (string-ref sfx sfx-idx)))
          #f
          (loop (sub1 idx) (sub1 sfx-idx)))))))

(define (read-all path)
  (define rev-result '())
  (load path)
        ;(lambda (form)
        ;  (set! rev-result (cons (expand form) rev-result))))
  (reverse rev-result))

(define (time-apply f)
  (define start-times (process-times))
  (define result (f))
  (define end-times (process-times))
  (define differ (lambda (i) (- (f64vector-ref end-times i) (f64vector-ref start-times i))))
  (list result (differ 2) (+ (differ 0) (differ 1))))

(define (benchmark f)
  ;(collect)
  (time-apply f))

(define args (cdr (command-line)))
(when (not (eqv? 1 (length args)))
  (error "expected one argument, got " (length args) ": " args))
(define benchmark-dir (car args))
(if (not (file-directory? benchmark-dir))
  (error "provided path does not exist or is not a directory: " benchmark-dir))
(define src-files 
  (map (lambda (v)
         (string-append benchmark-dir "/" v))
    (filter (lambda (v)
                (or (string-ends-with? v ".rkt") (string-ends-with? v ".scm")))
            (directory-files benchmark-dir))))

;(display (read-all (car src-files)))
(let loop ((rem src-files))
  (when (not (null? rem))
    (let* ((path (car rem))
           (result (benchmark (lambda () (read-all path))))
           (times (cdr result))
           (expanded (car result)))
      ;(printf "~a\n" expanded)
      ;(printf "~a: real=~as cpu=~as gc=~as\n" path (/ (sstats-real stats) 1000.0) (/ (sstats-cpu stats) 1000.0) (/ (sstats-gc-real stats) 1000.0))
      (print path ": real=" (car times) "s cpu=" (cadr times) "s gc=0s" "\n")
      (loop (cdr rem)))))
