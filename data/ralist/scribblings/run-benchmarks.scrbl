#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label data/ralist/run-benchmarks
                     data/ralist/benchmarks/ra-list
                     data/ralist/benchmarks/contract
                     data/ralist/benchmarks/freq-count
                     data/ralist/benchmarks/garden-fence))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require
                data/ralist/benchmarks/ra-list
                data/ralist/benchmarks/freq-count
                data/ralist/benchmarks/garden-fence))
    the-eval))

@title[#:tag "run-benchmarks"]{Benchmarks}

@defmodule[data/ralist/run-benchmarks]

Runs all of the benchmarks for this package.

@section{Random-access vs. Sequential-access lists}

@defmodule[data/ralist/benchmarks/ra-list]

This benchmark compares the performance of typical list operations
for random and sequential lists.

@defproc[(run-ra-list-benchmark) void?]{Runs this benchmark.}

@;examples[#:eval the-eval (run-ra-list-benchmark)]

@section[#:tag "benchmarks/contract"]{Contracted vs. Uncontracted bindings}

@defmodule[data/ralist/benchmarks/contract]

This benchmark compares the performance of the contracted and
uncontracted bindings.

@defproc[(run-contract-benchmark) void?]{Runs this benchmark.}

@section{Frequency counting}

@defmodule[data/ralist/benchmarks/freq-count]

This benchmark compares an number of imperative and functional solutions
to the problem of counting the frequencies of each number in a given 
list of numbers.

See the thread starting 
@link["http://list.cs.brown.edu/pipermail/plt-scheme/2009-April/032250.html"]{here} 
for discussion.

@defproc[(run-freq-count-benchmark) void?]{Runs this benchmark.}

@;examples[#:eval the-eval (run-freq-count-benchmark)]

@section{Garden fence encryption}

@defmodule[data/ralist/benchmarks/garden-fence]

This benchmark compares solutions to the problem of garden 
fence encryption.

Garden fence encryption works as follows: you are given a plain text message
(String) and a key (Nat).  You scramble the message by a process that depends
on the given key, producing a cipher text message (String) of the same length 
as the given plain text message.  The scrambled message can be de-scrambled 
to obtain the original message by an inverse process when it is given the 
same key.

@defproc[(encrypt [s string?] [k natural-number/c]) string?]{
Produce the cipher text of the given string using the given key.}

@defproc[(decrypt [s string?] [k natural-number/c]) string?]{
Produce the plain text of the given string using the given key.}
@examples[#:eval the-eval 
                 (encrypt "diesisteinklartext" 6)
                 (decrypt "dkinleiasertittxse" 6)]

The process of scrambling a message works in a zigzag form.  The
key gives the number of lines to the zigzag.  So suppose we want 
to encrypt the message @racket["diesisteinklartext"] with the key 
@racket[6].  Imagine the characters of the string are arranged in
a zigzag, or wave, or even @italic{fence}-like pattern, where the 
height of the wave, or zigzagging fency thing is @racket[6]:
@verbatim[#<<HERE
;;;; 1. d         k         = (d k)     = "dk"
;;;; 2.  i       n l        = (i n l)   = "inl"
;;;; 3.   e     i   a       = (e i a)   = "eia"
;;;; 4.    s   e     r   t  = (s e r t) = "sert"
;;;; 5.     i t       t x   = (i t t x) = "ittx"
;;;; 6.      s         e    = (s e)     = "se"
HERE
]
The characters are grouped by line, forming the lists above.  The
lists are appended in order to obtain the resulting cipher text:
@racket["dkinleiasertittxse"].



The following solutions are included in the benchmark:
@itemize{
  @item{An imperative, vector-based algorithm.}
  @item{A functional translation of the above using random-access lists.}
  @item{A functional algorithm designed by output structure.}
  @item{A combinator style algorithm.}
  @item{A cyclic sequence algorithm.}
}

See the thread starting 
@link["http://list.cs.brown.edu/pipermail/plt-scheme/2009-March/031310.html"]{here} 
and
@link["http://list.cs.brown.edu/pipermail/plt-dev/2009-April/000522.html"]{here}
for discussion.

@defproc[(run-garden-fence-benchmark) void?]{Runs this benchmark.}

@;examples[#:eval the-eval (run-garden-fence-benchmark)]
