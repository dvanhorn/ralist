#lang setup/infotab
(define name "Purely Functional Random-Access Lists.")
(define scribblings '(("scribblings/ralist.scrbl" (multi-page))))
(define categories '(datastructures))
(define required-core-version "5.1.1")
(define repositories (list "4.x"))
(define primary-file 
  '("main.ss" "contract.ss"))
(define blurb
  (list '(div "Purely Functional Random-Access Lists.")))
(define release-notes 
  (list
   '(div "Improved performance of " (tt "last") ".")))
