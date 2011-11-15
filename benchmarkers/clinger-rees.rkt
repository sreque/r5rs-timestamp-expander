#lang racket
(require
 "../clinger-rees-parser.rkt"
 "../clinger-rees-env.rkt"
 "../test/test-utils.rkt")

(make-expand-test-defs)

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

(define args (current-command-line-arguments))
(define arg-len (vector-length args))

(when (not (eqv? 1 arg-len))
  (error (format "expected exactly one argument: got ~a" arg-len)))

(define benchmark-dir (simplify-path (path->complete-path (vector-ref args 0))))

(define sources 
  (filter
   (compose not void?)
   (for/list ([path (in-directory benchmark-dir)])
     (if (regexp-match? #rx"[.](scm|rkt)$" path)
         (cons path (call-with-input-file path read-all))          
         (void)))))

#;(printf "~a\n" sources)

(for ([v sources])
  (match-define (cons path code) v)
  (define-values (_ cpu real gc) (benchmark (λ () (call-with-values (λ () (expand-program r5rs-top-level-env code)) list)) 0))
  (printf "~a: real=~as cpu=~as gc=~as\n" (find-relative-path benchmark-dir path) (/ real 1000.0) (/ cpu 1000.0) (/ gc 1000.0)))
  
#;(define sieve-code 
  (call-with-input-file
   "benchmark_sources/sieve-stress-test.scm"
   read-all))

#;(define-values (_ cpu real gc) (benchmark (λ () (expand-expr-list sieve-code)) 5))
#;(printf "sieve test: real=~as cpu=~as gc=~as\n" (/ real 1000.0) (/ cpu 1000.0) (/ gc 1000.0))
