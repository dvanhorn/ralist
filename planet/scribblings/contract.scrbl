#lang scribble/doc
@(require scribble/manual
          planet/scribble
          (only-in (for-label scheme) 
                   for sequence? lambda < and procedure? 
                   procedure-arity-includes?)
          (for-label scheme/contract)
          (only-in (for-label (this-package-in contract))
                   count=/c count>/c is-true/c arity-includes/c)
          (only-in (for-label (this-package-in main))
                   build-list map count))

@title[#:tag "contract"]{Contract}

@defmodule/this-package[contract]

Just like @racketmodname/this-package[main], but with contracts.

@defproc[(count=/c [n natural-number/c]) flat-contract?]{
Returns a flat contract that requires the input to have a count
equal to @scheme[n].}

@defproc[(count>/c [n natural-number/c]) flat-contract?]{
Returns a flat contract that requires the input to have a count
greater than @scheme[n].}

@defproc[(is-true/c [x any/c]) flat-contract?]{
Returns a flat contract that requires nothing of its input,
and returns @scheme[x], i.e. it produces a contract for
the predicate @scheme[(lambda (_) x)].}

@defproc[(arity-includes/c [n natural-number/c]) flat-contract?]{
Returns a flat contract that requires its input to be a procedure
accepting at least @scheme[n] arguments, i.e. it produces a contract
for the predicate,
@schemeblock[
  (lambda (x) 
    (and (procedure? x)
         (procedure-arity-includes? x n)))]}