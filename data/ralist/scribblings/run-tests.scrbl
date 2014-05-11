#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label rackunit
                     rackunit/text-ui
                     data/ralist/run-tests
                     data/ralist/tests/tree
                     data/ralist/tests/ra-list
                     data/ralist/tests/garden-fence
                     data/ralist/tests/freq-count))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require
                rackunit
                rackunit/text-ui
                data/ralist/tests/tree
                data/ralist/tests/ra-list
                data/ralist/tests/garden-fence
                data/ralist/tests/freq-count))
    the-eval))

@title[#:tag "run-tests"]{Tests}

@defmodule[data/ralist/run-tests]

Runs all unit tests for this package.

@section{Tree tests}

@defmodule[data/ralist/tests/tree]

@defthing[tree-tests test-suite?]

@examples[#:eval the-eval (run-tests tree-tests)]

@section{RaList tests}

@defmodule[data/ralist/tests/ra-list]

@defthing[ra-list-tests test-suite?]

@examples[#:eval the-eval (run-tests ra-list-tests)]

@section{Garden fence tests}

@defmodule[data/ralist/tests/garden-fence]

@defthing[garden-fence-tests test-suite?]

@examples[#:eval the-eval (run-tests garden-fence-tests)]

@section{Frequency counting tests}

@defmodule[data/ralist/tests/freq-count]

@defthing[freq-count-tests test-suite?]

@examples[#:eval the-eval (run-tests freq-count-tests)]
