3026
((3) 0 () 3 ((q lib "data/ralist/contract.rkt") (q lib "data/ralist.rkt") (q lib "data/ralist/benchmarks/garden-fence.rkt")) () (h ! (equal) ((q def ((lib "data/ralist/tests/ra-list.rkt") ra-list-tests)) q (4089 . 2)) ((c def c (c (? . 2) q decrypt)) q (4428 . 4)) ((q def ((lib "data/ralist/tests/tree.rkt") tree-tests)) q (4056 . 2)) ((c def c (c (? . 1) q fourth)) q (1575 . 3)) ((c def c (c (? . 0) q count>/c)) q (3845 . 3)) ((c def c (c (? . 0) q is-true/c)) q (3916 . 3)) ((c def c (c (? . 1) q count)) q (3456 . 3)) ((c def c (c (? . 1) q foldr)) q (2368 . 6)) ((c def c (c (? . 1) q tenth)) q (2023 . 3)) ((c def c (c (? . 1) q list-ref/set)) q (1165 . 5)) ((c def c (c (? . 1) q second)) q (1426 . 3)) ((c def c (c (? . 1) q build-list)) q (3245 . 5)) ((c def c (c (? . 1) q cons)) q (133 . 4)) ((c def c (c (? . 1) q car+cdr)) q (688 . 3)) ((c def c (c (? . 1) q empty)) q (110 . 2)) ((c def c (c (? . 1) q ninth)) q (1949 . 3)) ((c def c (c (? . 1) q cdr)) q (791 . 3)) ((c def c (c (? . 1) q andmap)) q (2750 . 5)) ((c def c (c (? . 1) q list-update)) q (1036 . 5)) ((c form c (c (? . 1) q for/list)) q (52 . 2)) ((c def c (c (? . 1) q reverse)) q (3722 . 3)) ((c def c (c (? . 1) q rest)) q (623 . 3)) ((c def c (c (? . 0) q arity-includes/c)) q (3977 . 3)) ((c def c (c (? . 1) q list?)) q (430 . 3)) ((c def c (c (? . 1) q sixth)) q (1724 . 3)) ((c def c (c (? . 1) q map)) q (2162 . 5)) ((c def c (c (? . 0) q count=/c)) q (3774 . 3)) ((c def c (c (? . 1) q list)) q (199 . 3)) ((c def c (c (? . 1) q first)) q (559 . 3)) ((c def c (c (? . 1) q fifth)) q (1650 . 3)) ((c def c (c (? . 2) q run-garden-fence-benchmark)) q (4512 . 2)) ((c def c (c (? . 1) q list-ref/update)) q (1286 . 5)) ((c def c (c (? . 2) q encrypt)) q (4344 . 4)) ((c def c (c (? . 1) q list-tail)) q (3517 . 4)) ((c def c (c (? . 1) q third)) q (1501 . 3)) ((c def c (c (? . 1) q last)) q (2097 . 3)) ((c def c (c (? . 1) q make-list)) q (3163 . 4)) ((c def c (c (? . 1) q car)) q (747 . 3)) ((c def c (c (? . 1) q list-set)) q (926 . 5)) ((c def c (c (? . 1) q empty?)) q (327 . 3)) ((c def c (c (? . 1) q in-list)) q (0 . 3)) ((q def ((lib "data/ralist/tests/freq-count.rkt") freq-count-tests)) q (4166 . 2)) ((c def c (c (? . 1) q list-ref)) q (835 . 4)) ((c def c (c (? . 1) q ormap)) q (2957 . 5)) ((c def c (c (? . 1) q cons?)) q (379 . 3)) ((c def c (c (? . 1) q eighth)) q (1874 . 3)) ((c def c (c (? . 1) q length)) q (3394 . 3)) ((q def ((lib "data/ralist/tests/garden-fence.rkt") garden-fence-tests)) q (4125 . 2)) ((q def ((lib "data/ralist/benchmarks/contract.rkt") run-contract-benchmark)) q (4250 . 2)) ((c def c (c (? . 1) q foldl)) q (2559 . 6)) ((c def c (c (? . 1) q seventh)) q (1798 . 3)) ((q def ((lib "data/ralist/benchmarks/ra-list.rkt") run-ra-list-benchmark)) q (4205 . 2)) ((c def c (c (? . 1) q append)) q (3601 . 6)) ((q def ((lib "data/ralist/benchmarks/freq-count.rkt") run-freq-count-benchmark)) q (4296 . 2)) ((c def c (c (? . 1) q list*)) q (251 . 4)) ((c def c (c (? . 1) q first+rest)) q (481 . 3))))
procedure
(in-list xs) -> list?
  xs : list?
syntax
(for/list ([id sequence-expr] ...) body ...+)
value
empty : empty?
procedure
(cons x y) -> cons?
  x : any/c
  y : any/c
procedure
(list x ...) -> list?
  x : any/c
procedure
(list* x ... tail) -> any
  x : any/c
  tail : any/c
procedure
(empty? x) -> boolean?
  x : any/c
procedure
(cons? x) -> boolean?
  x : any/c
procedure
(list? x) -> boolean?
  x : any/c
procedure
(first+rest xs) -> any/c list?
  xs : (and/c cons? list?)
procedure
(first xs) -> any
  xs : (and/c cons? list?)
procedure
(rest xs) -> list?
  xs : (and/c cons? list?)
procedure
(car+cdr xs) -> any/c any/c
  xs : cons?
procedure
(car p) -> any
  p : cons?
procedure
(cdr p) -> any
  p : cons?
procedure
(list-ref xs i) -> any/c
  xs : (count>/c i)
  i : natural-number/c
procedure
(list-set xs i x) -> cons?
  xs : (count>/c i)
  i : natural-number/c
  x : any/c
procedure
(list-update xs i f) -> cons?
  xs : (count>/c i)
  i : natural-number/c
  f : (arity-includes/c 1)
procedure
(list-ref/set xs i v) -> any/c cons?
  xs : (count>/c i)
  i : natural-number/c
  v : any/c
procedure
(list-ref/update xs i f) -> any/c cons?
  xs : (count>/c i)
  i : natural-number/c
  f : (arity-includes/c 1)
procedure
(second xs) -> any/c
  xs : (and/c list? (count>/c 1))
procedure
(third xs) -> any/c
  xs : (and/c list? (count>/c 2))
procedure
(fourth xs) -> any/c
  xs : (and/c list? (count>/c 3))
procedure
(fifth xs) -> any/c
  xs : (and/c list? (count>/c 4))
procedure
(sixth xs) -> any/c
  xs : (and/c list? (count>/c 5))
procedure
(seventh xs) -> any/c
  xs : (and/c list? (count>/c 6))
procedure
(eighth xs) -> any/c
  xs : (and/c list? (count>/c 7))
procedure
(ninth xs) -> any/c
  xs : (and/c list? (count>/c 8))
procedure
(tenth xs) -> any/c
  xs : (and/c list? (count>/c 9))
procedure
(last xs) -> any/c
  xs : (and/c cons? list?)
procedure
(map f xs ...+) -> list?
  f : (or/c (is-true/c (zero? (count xs)))
            (arity-includes/c (add1 (mz:length ...))))
  xs : (and/c list? (count=/c (count xs)))
procedure
(foldr f b xs ...+) -> any
  f : (or/c (is-true/c (zero? (count xs)))
            (arity-includes/c (+ 2 (mz:length ...))))
  b : any/c
  xs : list?
procedure
(foldl f a xs ...+) -> any
  f : (or/c (is-true/c (zero? (count xs)))
            (arity-includes/c (+ 2 (mz:length ...))))
  a : any/c
  xs : list?
procedure
(andmap f xs ...+) -> any
  f : (or/c (is-true/c (zero? (count xs)))
            (arity-includes/c (add1 (mz:length ...))))
  xs : (and/c list? (count=/c (count xs)))
procedure
(ormap f xs ...+) -> any
  f : (or/c (is-true/c (zero? (count xs)))
            (arity-includes/c (add1 (mz:length ...))))
  xs : (and/c list? (count=/c (count xs)))
procedure
(make-list n x) -> list?
  n : natural-number/c
  x : any/c
procedure
(build-list n f) -> list?
  n : natural-number/c
  f : (or/c (is-true/c (zero? n))
            (arity-includes/c 1))
procedure
(length xs) -> natural-number/c
  xs : list?
procedure
(count xs) -> natural-number/c
  xs : any/c
procedure
(list-tail xs i) -> list?
  xs : list?
  i : natural-number/c
procedure
(append xs ...) -> list?
  xs : list?
(append xs ... v) -> any/c
  xs : list?
  v : any/c
procedure
(reverse xs) -> list?
  xs : list?
procedure
(count=/c n) -> flat-contract?
  n : natural-number/c
procedure
(count>/c n) -> flat-contract?
  n : natural-number/c
procedure
(is-true/c x) -> flat-contract?
  x : any/c
procedure
(arity-includes/c n) -> flat-contract?
  n : natural-number/c
value
tree-tests : test-suite?
value
ra-list-tests : test-suite?
value
garden-fence-tests : test-suite?
value
freq-count-tests : test-suite?
procedure
(run-ra-list-benchmark) -> void?
procedure
(run-contract-benchmark) -> void?
procedure
(run-freq-count-benchmark) -> void?
procedure
(encrypt s k) -> string?
  s : string?
  k : natural-number/c
procedure
(decrypt s k) -> string?
  s : string?
  k : natural-number/c
procedure
(run-garden-fence-benchmark) -> void?
