#lang scheme
(require planet/version
         rackunit
         (this-package-in tests/private/ra-list-contracted)
         (this-package-in tests/private/ra-list-uncontracted))

(define/provide-test-suite ra-list-tests 
  ra-list-contracted-tests
  ra-list-uncontracted-tests)

