#lang scribble/manual
@(require ;scribble/manual
          (for-label data/ralist))

@title{RaList: Purely Functional Random-Access Lists}

@author+email["David Van Horn" "dvanhorn@cs.umd.edu"]

Random-access lists are a purely functional data structure for representing lists 
of values. A random-access list may act as a drop in replacement for the usual 
sequential list data structure (@racket[cons?], @racket[cons], @racket[car], 
@racket[cdr]), which additionally supports fast index-based addressing 
and updating (@racket[list-ref], @racket[list-set]).  

This document outlines the API for the random-access list 
library.  This implementation is based on Okasaki, FPCA '95.

@include-section["bindings.scrbl"]
@include-section["contract.scrbl"]
@include-section["run-tests.scrbl"]
@include-section["run-benchmarks.scrbl"]
@index-section[]
