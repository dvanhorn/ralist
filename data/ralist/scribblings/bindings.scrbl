#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          (only-in (for-label racket)
                   lambda for for/fold sequence? >= < + 
                   min sub1 add1 positive? or
                   zero? values sqrt)
          (prefix-in mz: (only-in (for-label racket) length))
          (for-label racket/contract)
          (for-label data/ralist)
          (only-in (for-label data/ralist/contract)
                   count=/c count>/c is-true/c))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require data/ralist))
    #;(the-eval '(error-print-width 180))
    the-eval))

@(define the-eval/co
  (let ([the-eval (make-base-eval)])
    (the-eval `(require data/ralist/contract))
    #;(the-eval '(error-print-width 180))
    the-eval))

@(define the-eval/seq
  (let ([the-eval (make-base-eval)])
    (the-eval `(require racket/list))
    #;(the-eval '(error-print-width 180))
    the-eval))

@(define-syntax failures 
   (syntax-rules ()
     [(failures e ...)
      (begin (make-paragraph '("Failures with contracts:")) 
             (interaction #:eval the-eval/co e ...)
             (make-paragraph '("Failures without contracts:")) 
             (interaction #:eval the-eval e ...))]))

@title[#:tag "main"]{Random-Access Lists}

@defmodule[data/ralist]

@margin-note{In addition to @racketmodname[data/ralist], there is a
@racketmodname[data/ralist/contract] module that provides the same
bindings but with checked contracts.  The only difference is
performance and error checking.  See
@secref{Checked_and_Unchecked_contracts} for details.}

Random-access lists look and behave much like their sequential access
counterparts.  The main difference is that @racket[list-ref] and
@racket[list-set] are logarithmic, rather than linear, in the size of
the list:
@interaction[#:eval the-eval
(list 0 1 2 3)
(cons 1 2)
(car (cons 1 2))
(cdr (cons 1 2))
(map (lambda (i) (/ 1 i))
     (list 1 2 3))
(list-ref (list 'x 'y 'z) 2)
(list-set (list 'x 'y 'z) 2 'q)
]

This can have a big impact on efficiency of accessing elements in a
list by their index.  For example, getting the @racket[last] element
of a sequential list takes time proportional to the length of the
list, but can be done much faster with random-access lists:

@interaction[#:eval the-eval
(module m racket/base
  (require data/ralist)
  (define ls (build-list 100000 values))
  (time (for ([i 1000]) (last ls))))
(require 'm)
]

Compare this with the timing for sequential lists:
@interaction[#:eval the-eval/seq
(module n racket/base
  (require racket/list)
  (define ls (build-list 100000 values))
  (time (for ([i 1000]) (last ls))))
(require 'n)
]

On the other hand, while list operations such as @racket[cons],
@racket[car], and @racket[cdr] take constant time just like their
sequential-list counterparts, the constant factors are usually worse.
In particular, @racket[cdr] may allocate memory.  For example, here is the same
program, but @racket[last] is computed by iteratively taking the
@racket[cdr] of the list instead of @racket[(list-ref xs (sub1 (length xs)))]:
@interaction[#:eval the-eval
(module o racket/base
  (require data/ralist)
  (define ls (build-list 100000 values))  
  (define (last xs) 
    (if (empty? (cdr xs)) (car xs) (last (cdr xs))))
  (time (for ([i 1000]) (last ls))))
(require 'o)]

Compare this with the performance of using sequential lists:
@interaction[#:eval the-eval
(module p racket/base
  (require racket/list)
  (define ls (build-list 100000 values))  
  (define (last xs) 
    (if (empty? (cdr xs)) (car xs) (last (cdr xs))))
  (time (for ([i 1000]) (last ls))))
(require 'p)]

There are several benchmarks described in the @secref{run-benchmarks}
section comparing the performance of random-access lists to
sequential-access lists as well as mutable data structures such as
vectors.  In general, random-access lists are well-suited for
situations requiring variable-length lists that are mostly accessed by
indexed.

@section{Pairs and Lists}

A @italic{pair} combines exactly two values. The first value is accessed with 
the @racket[car] procedure, and the second value is accessed with the 
@racket[cdr] procedure.  Pairs are not mutable.

A @italic{list} is recursively defined: it is either the constant @racket[empty], 
or it is a pair whose second value is a list.

A list can be used as a single-valued sequence (see 
@secref["sequences" #:doc '(lib "scribblings/reference/reference.scrbl")]).
The elements of the list serve as elements of the sequence. 
See also @racket[in-list].

@section{Sequences}

Random-access lists implement the sequence interface, so @racket[(list? x)] 
implies @racket[(sequence? x)], and elements of a list may be extracted with
any of the @racket[for] syntactic forms.

@defproc[(in-list [xs list?]) list?]{
Returns a sequence equivalent to @racket[xs].  Since lists are sequences,
this is a list identity function, but an @racket[in-list] application can
provide better performance when it appears directly in a @racket[for] clause.}

@examples[#:eval the-eval
                 (in-list (list 1 2 3))
                 (for/fold ([sum 0]
                            [rev-roots empty])
                   ([i (in-list (list 1 2 3 4))])
                   (values (+ sum i) (cons (sqrt i) rev-roots)))]

@section{Iterations and Comprehensions}

@defform[(for/list ([id sequence-expr] ...) body ...+)]{
Iterates like @racket[for], but that the last expression in 
the bodys must produce a single value, and the result of the 
@racket[for/list] expression is a list of the results in order.}

@examples[#:eval the-eval
                 (for/list ([i '(1 2 3)]
                            [j "abc"]
                            #:when (odd? i)
                            [k #(#t #f)])
                   (list i j k))
                 (for/list () 'any)
                 (for/list ([i '()])
                   (error "doesn't get here"))]

@section{Values}

@defthing[empty empty?]{
The empty list.}

@examples[#:eval the-eval
                 empty]

@defproc[(cons [x any/c] [y any/c]) cons?]{
Cons @racket[x] onto @racket[y].  When @racket[(list? y)], 
@racket[(cons x y)] constructs a list.}

@examples[#:eval the-eval
                 (cons 'x empty)
                 (cons 'x (cons 'y empty))
                 (cons 'x 'y)]

@defproc[(list [x any/c] ...) list?]{
Returns a list containing the given @racket[x]s as its elements.}

@examples[#:eval the-eval
                 (list)
                 (list 'x 'y 'z)]

@defproc[(list* [x any/c] ... [tail any/c]) any]{
Returns a list containing the given @racket[x]s as its elements
and @racket[tail] as its tail.  When @racket[(list? tail)], 
@racket[(list* x ... tail)] constructs a list.}

@examples[#:eval the-eval
                 (list* empty)
                 (list* 'x 'y (list 'p 'q))
                 (list* 1 2 3)
                 (list* 1)]

@defproc[(empty? [x any/c]) boolean?]{
Is @racket[x] the empty list?}

@examples[#:eval the-eval
                 (empty? empty)
                 (empty? (list))
                 (empty? (cons 'x empty))
                 (empty? 'x)]

@defproc[(cons? [x any/c]) boolean?]{
Is @racket[x] a pair?}

@examples[#:eval the-eval
                 (cons? empty)
                 (cons? (list))
                 (cons? (cons 'x empty))
                 (cons? 'x)]

@defproc[(list? [x any/c]) boolean?]{
Is @racket[x] a list?  Takes O(@racket[(log2 (count x))]).}

@examples[#:eval the-eval
                 (list? empty)
                 (list? (list))
                 (list? (cons 'x empty))
                 (list? 'x)]

@defproc[(first+rest [xs (and/c cons? list?)]) (values any/c list?)]{
The anti-@racket[cons] for lists.}

@examples[#:eval the-eval
                 (first+rest (list 1 2 3))]
@failures[(first+rest empty)
          (first+rest (cons 1 2))]

@defproc[(first [xs (and/c cons? list?)]) any]{
Returns the first element of the list @racket[xs].}

@examples[#:eval the-eval
                 (first (cons 'x empty))
                 (first (list 1 2 3))]
@failures[(first empty)
          (first (cons 'x 'y))]

@defproc[(rest [xs (and/c cons? list?)]) list?]{
Returns the rest of the element of the list @racket[xs].}

@examples[#:eval the-eval
                 (rest (cons 'x empty))
                 (rest (list 1 2 3))]
@failures[(rest empty)
          (rest (cons 'x 'y))]

@defproc[(car+cdr [xs cons?]) (values any/c any/c)]{
The anti-@racket[cons] for pairs.}

@examples[#:eval the-eval
                 (car+cdr (cons 1 2))
                 (car+cdr (list 1 2 3))]
@failures[(car+cdr empty)]

@defproc[(car [p cons?]) any]{
Returns the first component of the pair @racket[p].}

@examples[#:eval the-eval
                 (car (cons 1 2))
                 (car (list 1 2 3))]
@failures[(car empty)]

@defproc[(cdr [p cons?]) any]{
Returns second component of the pair @racket[p].}

@examples[#:eval the-eval
                 (cdr (cons 1 2))
                 (cdr (list 1 2 3))]
@failures[(cdr empty)]
                 
@defproc[(list-ref [xs (count>/c i)] 
                   [i natural-number/c]) 
         any/c]{
Returns the element of @racket[xs] at position @racket[i].  
This operation runs in O(@racket[(min i (log2 (count xs)))]).}

@examples[#:eval the-eval
                 (list-ref (list 'x 'y 'z) 0)
                 (list-ref (list 'x 'y 'z) 1)
                 (list-ref (list 'x 'y 'z) 2)
                 (list-ref (list* 'x 'y 'z) 0)]
@failures[(list-ref (list 'x 'y 'z) 3)
          (list-ref (list* 'x 'y 'z) 2)]

@defproc[(list-set [xs (count>/c i)] 
                   [i natural-number/c] 
                   [x any/c]) 
         cons?]{
Returns a chain of pairs identical to @racket[xs], except @racket[x] 
is the @racket[i]th element.  This operation runs in 
O(@racket[(min i (log2 (count xs)))]).}

@examples[#:eval the-eval
                 (list-set (list 'x 'y 'z) 0 'a)
                 (list-set (list 'x 'y 'z) 1 'b)
                 (list-set (list 'x 'y 'z) 2 'c)
                 (list-set (list* 'x 'y 'z) 0 'a)]
@failures[(list-set (list 'x 'y 'z) 3 'd)
          (list-set (list* 'x 'y 'z) 2 'c)]

@defproc[(list-update [xs (count>/c i)] 
                      [i natural-number/c] 
                      [f (procedure-arity-includes/c 1)])
         cons?]{
Returns @racket[(list-set xs i (f (list-ref xs i)))].} 

@defproc[(list-ref/set [xs (count>/c i)] 
                       [i natural-number/c] 
                       [v any/c]) 
         (values any/c cons?)]{
Returns @racket[(values (list-ref xs i) (list-set xs i v))], but is more
efficient.}
                              
@defproc[(list-ref/update [xs (count>/c i)] 
                          [i natural-number/c] 
                          [f (procedure-arity-includes/c 1)])
         (values any/c cons?)]{
Returns @racket[(values (list-ref xs i) (list-set xs i (f (list-ref xs i))))], 
but is more efficient.}

@defproc[(second [xs (and/c list? (count>/c 1))]) any/c]{
Returns the second element of the list.}

@defproc[(third [xs (and/c list? (count>/c 2))]) any/c]{
Returns the third element of the list.}

@defproc[(fourth [xs (and/c list? (count>/c 3))]) any/c]{
Returns the fourth element of the list.}

@defproc[(fifth [xs (and/c list? (count>/c 4))]) any/c]{
Returns the fifth element of the list.}

@defproc[(sixth [xs (and/c list? (count>/c 5))]) any/c]{
Returns the sixth element of the list.}

@defproc[(seventh [xs (and/c list? (count>/c 6))]) any/c]{
Returns the seventh element of the list.}

@defproc[(eighth [xs (and/c list? (count>/c 7))]) any/c]{
Returns the eighth element of the list.}

@defproc[(ninth [xs (and/c list? (count>/c 8))]) any/c]{
Returns the ninth element of the list.}

@defproc[(tenth [xs (and/c list? (count>/c 9))]) any/c]{
Returns the tenth element of the list.}
                     
@examples[#:eval the-eval
                 (second  (list 'a 'b))
                 (third   (list 'a 'b 'c)) 
                 (fourth  (list 'a 'b 'c 'd))
                 (fifth   (list 'a 'b 'c 'd 'e))
                 (sixth   (list 'a 'b 'c 'd 'e 'f)) 
                 (seventh (list 'a 'b 'c 'd 'e 'f 'g))
                 (eighth  (list 'a 'b 'c 'd 'e 'f 'g 'h))
                 (ninth   (list 'a 'b 'c 'd 'e 'f 'g 'h 'i)) 
                 (tenth   (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))]
@failures[(second (list* 'a 'b 'c))
          (second (list 'a))]

@defproc[(last [xs (and/c cons? list?)]) any/c]{
Returns the last element of the list.}

@examples[#:eval the-eval
                 (last (list 1 2 3))]
@failures[(last empty)
          (last (list* 1 2 3))]

@defproc[(map [f (or/c (is-true/c (zero? (count xs)))
                       (procedure-arity-includes/c (add1 (mz:length ...))))]
              [xs (and/c list? (count=/c (count xs)))] 
              ...+)
         list?]{
Applies @racket[f] to each element of @racket[xs]s from the first element
to the last.  The @racket[f] argument must accept the same number of arguments
as the number of supplied @racket[xs]s.  The result is a list containing each 
result of @racket[f] in order.}

@examples[#:eval the-eval
                 (map 0 empty) 
                 (map add1 empty) 
                 (map add1 (list 0 1 2))
                 (map (lambda (x y) (+ x y)) (list 1 2 3) (list 4 5 6))]
@failures[(map add1 (list 1 2 3) (list 4 5 6))
          (map + (list 1 2 3) (list 4))]

@defproc[(foldr [f (or/c (is-true/c (zero? (count xs)))
                         (procedure-arity-includes/c (+ 2 (mz:length ...))))]
                [b any/c]
                [xs list?] ...+)
         any]{
Like @racket[foldr] but for random-access lists.}

@defproc[(foldl [f (or/c (is-true/c (zero? (count xs)))
                         (procedure-arity-includes/c (+ 2 (mz:length ...))))]
                [a any/c]
                [xs list?] ...+)
         any]{
Like @racket[foldl] but for random-access lists.}

@defproc[(andmap [f (or/c (is-true/c (zero? (count xs)))
                          (procedure-arity-includes/c (add1 (mz:length ...))))]
                 [xs (and/c list? (count=/c (count xs)))]  ...+) 
         any]{
Like @racket[andmap] but for random-access lists.}

@defproc[(ormap [f (or/c (is-true/c (zero? (count xs)))
                         (procedure-arity-includes/c (add1 (mz:length ...))))]
                [xs (and/c list? (count=/c (count xs)))]  ...+) 
         any]{
Like @racket[ormap] but for random-access lists.}

@defproc[(make-list [n natural-number/c] [x any/c]) list?]{
Returns a list with @racket[n] elements, all of which are @racket[x].
Equivalent to @racket[(build-list n (lambda (i) x))].}

@examples[#:eval the-eval
                 (make-list 0 'x)
                 (make-list 5 'x)]

@defproc[(build-list [n natural-number/c] 
                     [f (or/c (is-true/c (zero? n)) 
                              (procedure-arity-includes/c 1))]) 
         list?]{
Creates a list of @racket[n] elemenents by applying @racket[f]
to the integers from @racket[0] to @racket[(sub1 n)].}

@examples[#:eval the-eval
                 (build-list 0 'not-function)
                 (build-list 0 (lambda (i) i))
                 (build-list 10 values)
                 (build-list 5 (lambda (x) (* x x)))]
@failures[(build-list 5 'not-function)]
                 
@defproc[(length [xs list?]) natural-number/c]{
Returns the length of the list.  This operation runs in 
O(@racket[(log2 (length xs))]).}

@examples[#:eval the-eval
                 (length empty)
                 (length (list 1 2 3))]
@failures[(length (list* 1 2 3))]

@defproc[(count [xs any/c]) natural-number/c]{
Returns the number of cons chained together to form @racket[xs]. 
O(@racket[(log2 (count xs))]).}

@examples[#:eval the-eval
                 (count empty)
                 (count 'x)
                 (count (list 1 2 3))
                 (count (list* 1 2 3))]

@defproc[(list-tail [xs list?] [i natural-number/c]) list?]{
Returns the list after the first @racket[i] elements of @racket[xs].
This operation, like its pair counterpart runs in O(@racket[i]).}

@defproc*[[((append [xs list?] ...) list?)
           ((append [xs list?] ... [v any/c]) any/c)]]{
Returns a list with all the elements of the given lists
in order.}

@examples[#:eval the-eval
                 (append)
                 (append empty empty empty)
                 (append (list 1 2 3) (list 4) (list 5 6))
                 (append (list 1 2 3) 4)
                 (append 5)]
@failures[(append 3 5)
          (append 1 (list 2 3))]

@defproc[(reverse [xs list?]) list?]{
Returns a list with all the elements of @racket[xs] in reverse order.}

@examples[#:eval the-eval
                 (reverse empty)
                 (reverse (list 1 2 3))]
@failures[(reverse (list* 1 2 3))]

@defproc[(range [n natural-number/c]) list?]{
Returns a list of natural numbers ranging from 0 to @racket[n-1] in ascending order.}

@examples[#:eval the-eval
                 (range 0)
                 (range 10)]

@failures[(range (list 1 2 3))]

@section{Checked and Unchecked contracts}

This library provides bindings for list operations in two forms: the 
first @italic{assumes} all contracts are met, the second @italic{checks}.

The observable differences are in their failure modes---what happens 
when the contract is not met---and execution times.  While the unchecked 
operations are free to fail in any way (which includes not failing at all
in some cases), if the user of this library violates the contract, the 
checked bindings will fail with a contract violation exception with appropriate
blame.  On the other hand, the unchecked bindings avoid any
overhead associated with the contract check, which can be
significant.

The checked bindings are designed to be complete in their checking of
contract properties, regardless of computational expense.  The unchecked
bindings are designed to be fast and to give reasonable error messages to
any violation that can easily be detected.  Given inputs that satisfy
the contract, both versions produced equivalent results.

The main module provides bindings for list operations with unchecked 
contracts.  Where appropriate, examples are given demonstrating the 
differences in failure modes for the checked and unchecked bindings.
See the benchmark on @seclink{benchmarks/contract} for a performance
comparison.
