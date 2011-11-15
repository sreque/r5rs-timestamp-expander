(define (string-ends-with? str sfx)
  (define sfx-len (string-length sfx))
  (define str-len (string-length str))
  (if (< str-len sfx-len)
    #f
    (let loop ((idx (sub1 str-len)) 
               (sfx-idx (sub1 sfx-len)))
      (if (< 0 sfx-idx)
        #t
        (if (not (char=? (string-ref str idx) (string-ref sfx sfx-idx)))
          #f
          (loop (sub1 idx) (sub1 sfx-idx)))))))

(define (read-all path)
  (define rev-result '())
  (load path
        (lambda (form)
          (set! rev-result (cons (expand form) rev-result))))
  (reverse rev-result))

(define (time-apply f)
  (define start-time (statistics))
  (define stat-time (statistics))
  (define result (f))
  (cons result (sstats-difference (sstats-difference  (statistics) start-time) (sstats-difference stat-time start-time))))

(define (benchmark f)
  (collect)
  (time-apply f))

(define args (cdr (command-line)))
(when (not (eqv? 1 (length args)))
  (errorf #f "expected one argument, got ~a: ~a" (length args) args))
(define benchmark-dir (car args))
(when (not (file-directory? benchmark-dir))
  (errorf #f "provided path does not exist or is not a directory: ~a" benchmark-dir))
(define src-files 
  (map (lambda (v)
         (string-append benchmark-dir (string (directory-separator)) v))
    (filter (lambda (v)
              (or (string-ends-with? v ".rkt") (string-ends-with? v ".scm")))
            (directory-list benchmark-dir))))

;(display (read-all (car src-files)))
(let loop ((rem src-files))
  (when (not (null? rem))
    (let* ((path (car rem))
           (result (benchmark (lambda () (read-all path))))
           (stats (cdr result))
           (expanded (car result)))
      ;(printf "~a\n" expanded)
      (printf "~a: real=~as cpu=~as gc=~as\n" path (/ (sstats-real stats) 1000.0) (/ (sstats-cpu stats) 1000.0) (/ (sstats-gc-real stats) 1000.0))
      (loop (cdr rem)))))
