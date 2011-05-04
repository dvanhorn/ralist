#lang racket
(require planet/version
         (this-package-in main)
         (this-package-in benchmarks/ra-list)
         (this-package-in benchmarks/contract)
         (this-package-in benchmarks/freq-count)
         (this-package-in benchmarks/garden-fence))

(run-ra-list-benchmark)
(run-freq-count-benchmark)
(run-garden-fence-benchmark)
(run-contract-benchmark)