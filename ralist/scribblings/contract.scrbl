#lang scribble/doc
@(require scribble/manual
          (only-in (for-label racket) 
                   for sequence? lambda < and procedure? 
                   procedure-arity-includes?)
          (for-label racket/contract)
          (only-in (for-label "../contract.rkt")
                   count=/c count>/c is-true/c arity-includes/c)
          (only-in (for-label "../contract.rkt")
                   build-list map count))

@title[#:tag "contract"]{Contract}

@defmodule[contract]

Just like @racketmodname[main], but with contracts.

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

@defproc[(arity-includes/c [n natural-number/c]) flat-contract?]{
Returns a flat contract that requires its input to be a procedure
accepting at least @racket[n] arguments, i.e. it produces a contract
for the predicate,
@racketblock[
  (lambda (x) 
    (and (procedure? x)
         (procedure-arity-includes? x n)))]}