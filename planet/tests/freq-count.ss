#lang scheme
(require ;planet/version
         rackunit)
(require/expose "../benchmarks/freq-count.ss" 
                ;(this-package-in benchmarks/freq-count)
                (gen cnt-ra.0 cnt-ra.1 cnt-vec cnt-alst cnt-BST cnt-ht))

(define str (gen 1000))

(define/provide-test-suite freq-count-tests
  (check-equal? (cnt-ra.0 str) (cnt-ra.1 str))
  (check-equal? (cnt-ra.0 str) (cnt-vec str))
  (check-equal? (cnt-ra.0 str) (cnt-alst str))
  (check-equal? (cnt-ra.0 str) (cnt-BST str))
  (check-equal? (cnt-ra.0 str) (cnt-ht str)))
