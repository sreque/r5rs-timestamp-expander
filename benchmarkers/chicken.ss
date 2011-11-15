(require 'posix)
(use filepath)
(use format)
(use environments)
(require-extension (srfi 1))
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
  (define env (environment-copy (interaction-environment)))
  (load path)
  ;I have to eval here because the expand procedure doesn't expand using user-defined macros, which makes it worthless for benchmarking
  ;(load path
        ;(lambda (form)
          ;(set! rev-result (cons 
                             ;(expand form)
                             ;;(begin (define expanded (expand form)) (display expanded) (newline) (eval expanded) expanded) 
                             ;rev-result))))
  (let ((result (reverse rev-result)))
    result))

(define (time-apply f)
  ;the cpu-time procedure is broken on windows 7.
  (define start-real (current-milliseconds))
  (define start-gc (current-gc-milliseconds))
  (define start-cpu start-real)
  ;(define-values (start-cpu-user start-cpu-sys) (cpu-time))
  ;(define start-cpu (+ start-cpu-user start-cpu-sys)) ;this is broken on windows 7
  (define result (f))
  (define end-real (current-milliseconds))
  (define end-gc (current-gc-milliseconds))
  (define end-cpu end-real)
  ;(define-values (end-cpu-user end-cpu-sys) (cpu-time))
  ;(define end-cpu (+ end-cpu-user end-cpu-sys)) ;This is broken on windows
  (list result (- end-real start-real) (- end-cpu start-cpu) (- end-gc start-gc)))

(define (benchmark f)
  (gc)
  (let ((time-to-do-nothing (time-apply (lambda () #f))))
    (gc)
    (let ((time-to-exec (time-apply f)))
      (cons (car time-to-exec) (map - (cdr time-to-exec) (cdr time-to-do-nothing))))))

(define args (command-line-arguments))
(when (not (eqv? 1 (length args)))
  (errorf #f "expected one argument, got ~a" (length args)))
(define benchmark-dir (car args))
(when (not (directory? benchmark-dir))
  (errorf #f "provided path does not exist or is not a directory: ~a" benchmark-dir))
(define src-files 
  (map (lambda (v)
         (string-append benchmark-dir (string (filepath:path-separator)) v))
    (filter (lambda (v)
              (or (string-ends-with? v ".rkt") (string-ends-with? v ".scm")))
            (directory benchmark-dir))))

(let loop ((rem src-files))
  (when (not (null? rem))
    (if (irregex-search "dirtier" (car rem))
      (loop (cdr rem))
      (let* ((path (car rem))
             (bench-results (benchmark (lambda () (read-all path))))
             (stats (cdr bench-results))
             (expanded (car bench-results)))
        ;(printf "path=~a\n\texpanded=~a\n" path  expanded)
        (format (current-output-port) "~a: real=~Fs cpu=~Fs gc=~Fs\n" path (/ (car stats) 1000.0) (/ (cadr stats) 1000.0) (/ (caddr stats) 1000.0))
        (loop (cdr rem)))))
  (exit)) ;If I don't call exit here, chicken scheme errors out with a message "unbound variable: main"
