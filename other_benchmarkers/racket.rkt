#lang racket
(require syntax/toplevel)
;There is a lot of copy-paste between this file and benchmark.rkt, since they both happen to be written in racket. 
;If either gets modified, consider abstracting the commonalities into a separate file.
(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'racket))

(define (read-all port)
  (let loop ((result null))
    (define next (read port))
    (if (eof-object? next)
        (reverse result)
        (loop (cons next result)))))

(define-syntax cfor
  (syntax-rules ()
    [(_ (var init cond incr) code ...)
     (let loop ([var init])
       (when cond
         (begin code ... (loop incr))))]))

(define (benchmark code warm-up-count)
  (cfor (i 0 (< i warm-up-count) (add1 i)) (code))
  (collect-garbage)
  (time-apply code null))

(define (expand-sexprs code)
  (let loop ([rem-code code])
    (if (null? rem-code)
        null
        (parameterize ([current-namespace ns])
          (cons (expand-top-level-with-compile-time-evals (datum->syntax #f (car rem-code))) (loop (cdr rem-code)))))))

;I couldn't figure out how to run this in drracket for debugging purposes without wrapping everything in parameterize
;(parameterize ([current-command-line-arguments (vector "../benchmark_sources")])
(define args (current-command-line-arguments))
(define arg-len (vector-length args))

(when (not (eqv? 1 arg-len))
  (error (format "expected exactly one argument: got ~a" arg-len)))

(define benchmark-dir (simplify-path (path->complete-path (vector-ref args 0))))

(when (not (directory-exists? benchmark-dir))
  (error (format "Specified path doesn't exist or is not a directory: ~a" (vector-ref args 0))))

;list of cons pairs, where the car is a path and the cdr is the parsed s-expressions in the file. 
(define sources 
  (filter
   (compose not void?)
   (for/list ([path (in-directory benchmark-dir)])
     (if (regexp-match? #rx"[.](scm|rkt)$" path)
         (cons path (call-with-input-file path read-all))          
         (void)))))

(for ([source sources])
  (match-define (cons path code) source)
  (define-values (expanded cpu real gc) (benchmark (λ () (call-with-values (λ () (expand-sexprs code)) list)) 0))
  (printf "~a: real=~as cpu=~as gc=~as\n" (find-relative-path benchmark-dir path) (/ real 1000.0) (/ cpu 1000.0) (/ gc 1000.0)))

  #;(for ([v expanded])
    (for ([_v v])
      (for ([__v _v])
        (display (syntax->datum __v)) (display (newline)))))
  ;)