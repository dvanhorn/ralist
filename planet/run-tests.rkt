#lang racket
(require planet/version
         rackunit
         rackunit/text-ui
         (this-package-in contract)
         (this-package-in tests/tree)
         (this-package-in tests/ra-list)
         (this-package-in tests/garden-fence)
         (this-package-in tests/freq-count))

(run-tests 
 (test-suite "All tests" 
             tree-tests 
             ra-list-tests
             garden-fence-tests
             freq-count-tests))
