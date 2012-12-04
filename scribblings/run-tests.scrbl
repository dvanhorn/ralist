#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label rackunit
                     rackunit/text-ui
                     "../run-tests.rkt"
                     "../tests/tree.rkt"
                     "../tests/ra-list.rkt"
                     "../tests/garden-fence.rkt"
                     "../tests/freq-count.rkt"))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require
                rackunit
                rackunit/text-ui
                "../tests/tree.rkt"
                "../tests/ra-list.rkt"
                "../tests/garden-fence.rkt"
                "../tests/freq-count.rkt"))
    the-eval))

@title[#:tag "run-tests"]{Tests}

@defmodule[run-tests]

Runs all unit tests for this package.

@section{Tree tests}

@defmodule[tests/tree]

@defthing[tree-tests test-suite?]

@examples[#:eval the-eval (run-tests tree-tests)]

@section{RaList tests}

@defmodule[tests/ra-list]

@defthing[ra-list-tests test-suite?]

@examples[#:eval the-eval (run-tests ra-list-tests)]

@section{Garden fence tests}

@defmodule[tests/garden-fence]

@defthing[garden-fence-tests test-suite?]

@examples[#:eval the-eval (run-tests garden-fence-tests)]

@section{Frequency counting tests}

@defmodule[tests/freq-count]

@defthing[freq-count-tests test-suite?]

@examples[#:eval the-eval (run-tests freq-count-tests)]
