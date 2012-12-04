#lang racket
(require (prefix-in ra: "../main.rkt")
         (prefix-in ls: racket))

(provide run-ra-list-benchmark)
(define (run-ra-list-benchmark)
  (define title "RAList v. List benchmark")
  (define module-path 'DONTKNOW #;(this-package-version-symbol))
  (define ps (list "ra" "mz"))
  (include (file "private/ra-list-common.rktl")))
