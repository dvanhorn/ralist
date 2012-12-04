#lang racket
(require rackunit
         "private/ra-list-contracted.rkt"
         "private/ra-list-uncontracted.rkt")

(define/provide-test-suite ra-list-tests 
  ra-list-contracted-tests
  ra-list-uncontracted-tests)

