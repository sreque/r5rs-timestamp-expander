#lang scheme
(require "timestamp-expander.rkt")

(printf "~a\n" (ts-syntax 'a 0))
(define stx1 (quote (a 1 a "b")))

(printf "~a\n" (timestamp-syntax stx1 0))