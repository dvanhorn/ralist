#lang racket
(require rackunit
         rackunit/text-ui
         "contract.rkt"
         "tests/tree.rkt"
         "tests/ra-list.rkt")
         "tests/garden-fence.rkt"
         "tests/freq-count.rkt")

(run-tests 
 (test-suite "All tests" 
             tree-tests 
             ra-list-tests
             garden-fence-tests
             freq-count-tests))
