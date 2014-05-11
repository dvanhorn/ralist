#lang scribble/doc
@(require scribble/manual
          (only-in (for-label racket) lambda)
          (for-label racket/contract)
          (only-in (for-label data/ralist/contract)
                   count=/c count>/c is-true/c))

@title[#:tag "contract"]{Contract}

@defmodule[data/ralist/contract]

@;Just like @racketmodname[main], but with contracts.

@defproc[(count=/c [n natural-number/c]) flat-contract?]{
Returns a flat contract that requires the input to have a count
equal to @racket[n].}

@defproc[(count>/c [n natural-number/c]) flat-contract?]{
Returns a flat contract that requires the input to have a count
greater than @racket[n].}

@defproc[(is-true/c [x any/c]) flat-contract?]{
Returns a flat contract that requires nothing of its input,
and returns @racket[x], i.e. it produces a contract for
the predicate @racket[(lambda (_) x)].}
