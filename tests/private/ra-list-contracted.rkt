#lang racket
(require rackunit
         "../../contract.rkt")

(define/provide-test-suite ra-list-contracted-tests
  (let-syntax ((check-fail
                    (syntax-rules ()
                      [(check-fail e)
                       (check-exn exn:fail:contract?
                                  (lambda () e))])))
    (include (file "ra-list-common.rktl"))))