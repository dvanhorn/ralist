#lang scribble/doc
@(require scribble/manual
          scribble/eval
          planet/scribble
          planet/version
          (for-label rackunit
                     rackunit/text-ui
                     (this-package-in run-tests)
                     (this-package-in tests/tree)
                     (this-package-in tests/ra-list)
                     (this-package-in tests/garden-fence)
                     (this-package-in tests/freq-count)))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require
                rackunit
                rackunit/text-ui
                (planet ,(this-package-version-symbol tests/tree))
                (planet ,(this-package-version-symbol tests/ra-list))
                (planet ,(this-package-version-symbol tests/garden-fence))
                (planet ,(this-package-version-symbol tests/freq-count))))
    the-eval))

@title[#:tag "run-tests"]{Tests}

@defmodule/this-package[run-tests]

Runs all unit tests for this package.

@section{Tree tests}

@defmodule/this-package[tests/tree]

@defthing[tree-tests test-suite?]

@examples[#:eval the-eval (run-tests tree-tests)]

@section{RaList tests}

@defmodule/this-package[tests/ra-list]

@defthing[ra-list-tests test-suite?]

@examples[#:eval the-eval (run-tests ra-list-tests)]

@section{Garden fence tests}

@defmodule/this-package[tests/garden-fence]

@defthing[garden-fence-tests test-suite?]

@examples[#:eval the-eval (run-tests garden-fence-tests)]

@section{Frequency counting tests}

@defmodule/this-package[tests/freq-count]

@defthing[freq-count-tests test-suite?]

@examples[#:eval the-eval (run-tests freq-count-tests)]
