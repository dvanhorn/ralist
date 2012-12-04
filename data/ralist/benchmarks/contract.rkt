#lang racket
(require (prefix-in ra: "../main.rkt")
         (prefix-in ls: "../contract.rkt")
         #;(prefix-in tr: (planet krhari/pfds:1:5/skewbinaryrandomaccesslist)))

;; The comparison to the typed/racket pfds implementation is too slow
;; to include.

(provide run-contract-benchmark)
(define (run-contract-benchmark)
  (define title "Contract v. Un benchmark")
  (define module-path 'DONTKNOW #;(this-package-version-symbol contract))
  (define ps (list "un" "co" #;"tr"))
  (include (file "private/ra-list-common.rktl")))
