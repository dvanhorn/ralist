#lang scheme
(require planet/version
         rackunit
         (this-package-in contract))

(define/provide-test-suite ra-list-contracted-tests
  (let-syntax ((check-fail
                    (syntax-rules ()
                      [(check-fail e)
                       (check-exn exn:fail:contract?
                                  (lambda () e))])))
    (include (file "ra-list-common.source"))))