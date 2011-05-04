#lang racket
(require planet/version
         rackunit
         (this-package-in main))

(define/provide-test-suite ra-list-uncontracted-tests
  (let-syntax ((check-fail
                (syntax-rules ()
                  [(check-fail e)
                   'ignore])))
    (include (file "ra-list-common.rktl"))))
