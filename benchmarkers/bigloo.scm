;This script doesn't work. Bigloo's hygienic macro expander appears to be broken.
;It fails to handle all of the benchmarks except the most trivial one.
(module bigloo-benchmark)
(define (sub1 x) (- x 1))
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
  (load path
        (lambda (form)
          (set! rev-result (cons (expand form) rev-result))))
  (reverse rev-result))

(define (benchmark f)
 ;(collect)
  (call-with-values (lambda () (time f)) (lambda (res rtime stime utime) (list res rtime (+ stime utime)))))

(define args (cdr (command-line)))
(when (not (eqv? 1 (length args)))
  (error "main" (format "expected one argument, got ~a: ~a" (length args) args) args))
(define benchmark-dir (car args))
(when (not (directory? benchmark-dir))
  (error "main" (format "provided path does not exist or is not a directory: ~a" benchmark-dir) benchmark-dir))
(define src-files 
  (map (lambda (v)
         (string-append benchmark-dir (string (file-separator)) v))
    (filter (lambda (v)
              (or (string-ends-with? v ".rkt") (string-ends-with? v ".scm")))
            (directory->list benchmark-dir))))

;(display (read-all (car src-files)))
(let loop ((rem src-files))
  (when (not (null? rem))
    (let* ((path (car rem))
           (res-list (benchmark (lambda () (read-all path)))))
      ;(printf "~a\n" expanded)
      (printf "~a: real=~as cpu=~as gc=~as\n" path (/ (cadr res-list) 1000.0) (/ (caddr res-list) 1000.0) "unknown")
      (loop (cdr rem)))))
