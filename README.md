Purely Functional Random-Access Lists
=====================================

Copyright (c) 2009 David Van Horn
Licensed under the Academic Free License version 3.0

`(at dvanhorn (dot cs umd edu))`

Random-access lists are a purely functional data structure for
representing lists of values. A random-access list may act as a drop
in replacement for the usual sequential list data structure (`cons?`,
`cons`, `car`, `cdr`), which additionally supports fast index-based
addressing and updating (`list-ref`, `list-set`).

Implementation based on Okasaki, FPCA '95.

Requires Racket v5.3.1.9 or later.

To install:

```
raco pkg install ralist
```
To use:

```
(require data/ralist)
```
