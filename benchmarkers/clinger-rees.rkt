#lang racket
(require
 "../clinger-rees-parser.rkt"
 "../clinger-rees-env.rkt"
 "../test/test-utils.rkt"
 profile
 (prefix-in profile:graphviz: profile/render-graphviz)
 (prefix-in profile:text: profile/render-text))

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

(define benchmark-path (simplify-path (path->complete-path (vector-ref args 0))))

(define sources
  (map (λ (path) (cons path (call-with-input-file path read-all)))
       (cond 
         [(directory-exists? benchmark-path)
          (filter
           (compose not void?)
           (for/list ([path (in-directory benchmark-path)])
             (if (regexp-match? #rx"[.](scm|rkt)$" path)
                 path          
                 (void))))]
         [(file-exists? benchmark-path)
          (list benchmark-path)]
         [else
          (error(format "path does not refer to a file or directory: ~a" benchmark-path))])))

#;(printf "~a\n" (for/list ([v sources]) (car v)))
#;(exit)
(define (main)
  (for ([v sources])
    (match-define (cons path code) v)
    (define-values (_ cpu real gc) (benchmark 
                                    (λ () (call-with-values 
                                           (λ () (expand-program r5rs-top-level-env code)) list)) 5))
    (printf "~a: real=~as cpu=~as gc=~as\n" (find-relative-path benchmark-path path) (/ real 1000.0) (/ cpu 1000.0) (/ gc 1000.0))))

(define (profile-main)
  (for ([v sources])
    (match-define (cons path code) v)
    (expand-program r5rs-top-level-env code)))

#;(profile-thunk profile-main
         #:delay 0.001
         #:repeat 100
         #:render profile:text:render)

(main)
(flush-output)
(flush-output)