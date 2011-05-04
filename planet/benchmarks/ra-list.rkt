#lang racket
(require planet/version
         (prefix-in ra: (this-package-in main))
         (prefix-in ls: racket))

(provide run-ra-list-benchmark)
(define (run-ra-list-benchmark)
  (define title "RAList v. List benchmark")
  (define module-path (this-package-version-symbol))
  (define ps (list "ra" "mz"))
  (include (file "private/ra-list-common.rktl")))
