#lang scheme
(require planet/version
         (prefix-in ra: (this-package-in main))
         (prefix-in ls: (this-package-in contract))
         #;(prefix-in tr: (planet krhari/pfds:1:5/skewbinaryrandomaccesslist)))

;; The comparison to the typed/racket pfds implementation is too slow
;; to include.

(provide run-contract-benchmark)
(define (run-contract-benchmark)
  (define title "Contract v. Un benchmark")
  (define module-path (this-package-version-symbol contract))
  (define ps (list "un" "co" #;"tr"))
  (include (file "private/ra-list-common.source")))
