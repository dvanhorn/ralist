#lang scribble/doc
@(require scribble/manual
          planet/util
          planet/scribble
          (for-label (this-package-in main)))

@title{@bold{RaList}: Purely Functional Random-Access Lists}

@author+email["David Van Horn" "dvanhorn@ccs.neu.edu"]

Random-access lists are a purely functional data structure for representing lists 
of values. A random-access list may act as a drop in replacement for the usual 
sequential list data structure (@scheme[cons?], @scheme[cons], @scheme[car], 
@scheme[cdr]), which additionally supports fast index-based addressing 
and updating (@scheme[list-ref], @scheme[list-set]).  

This document outlines the API for the random-access list 
library.  This implementation is based on Okasaki, FPCA '95.

@link[(format 
       "http://planet.racket-lang.org/trac/newticket?component=~a%2F~a&planetversion=(~a+~a)"
       (this-package-version-owner)
       (this-package-version-name)
       (this-package-version-maj)
       (this-package-version-min))]{Report a bug}.

@table-of-contents[]
@include-section["bindings.scrbl"]
@include-section["contract.scrbl"]
@include-section["run-tests.scrbl"]
@include-section["run-benchmarks.scrbl"]
@index-section[]
