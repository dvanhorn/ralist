#lang racket
(require "../ralist.rkt"
         "benchmarks/ra-list.rkt"
         "benchmarks/contract.rkt"
         "benchmarks/freq-count.rkt"
         "benchmarks/garden-fence.rkt")

(run-ra-list-benchmark)
(run-freq-count-benchmark)
(run-garden-fence-benchmark)
(run-contract-benchmark)