#lang racket
(require 
 rackunit
 rackunit/text-ui
 "clinger-rees-syntax-rules-test.rkt"
 "clinger-rees-parser-test.rkt"
 "ck-test.rkt"
 "dirty-r5rs-test.rkt"
 "sieve-stress-test.rkt"
 "schelog-test.rkt"
 "r5rs-ellipses-test.rkt"
 "macro-lambda-test.rkt"
 "dark-corner-test.rkt")

(define suites
  (list
   clinger-rees-syntax-rules-test
   clinger-rees-parser-test
   ck-test
   dark-corner-test
   dirty-r5rs-test
   macro-lambda-test
   r5rs-ellipses-test
   schelog-test
   sieve-stress-test))

(define one-suite-to-rule-them-all (make-test-suite "all tests" suites))

#;(run-tests (caddr suites))
(run-tests one-suite-to-rule-them-all)