#lang racket
#|               .__  .__          __            .__   __   
   ____________  |  | |__| _______/  |_   ______ |  |_/  |_ 
   \_  __ \__  \ |  | |  |/  ___/\   __\  \____ \|  |\   __\
    |  | \// __ \|  |_|  |\___ \  |  |    |  |_> >  |_|  |  
    |__|  (____  /____/__/____  > |__|   /\   __/|____/__|  
               \/             \/         \/__|              
   Purely Functional Random-Access Lists.

   Copyright (c) 2009 David Van Horn
   Licensed under the Academic Free License version 3.0

   (at dvanhorn (dot ccs neu edu))

   Implementation based on Okasaki, FPCA '95.                          

   Documentation, source code, bug tracking at:

      http://bit.ly/ralist

   To install:
      (require (planet dvanhorn/ralist))
   Or:
      racket -p dvanhorn/ralist

   To test, benchmark:
      (require (planet dvanhorn/ralist/run-tests))
      (require (planet dvanhorn/ralist/run-benchmarks))
   Or:
      racket -p dvanhorn/ralist/run-tests
      racket -p dvanhorn/ralist/run-benchmarks

   For contracted bindings, use:
      (require (planet dvanhorn/ralist/contract))

   Data definitions
   ----------------

   This library represents lists as an ordered sequence of full binary trees.
   The ordering is given by the height of each full binary tree, which is a 
   number of the form 2^i-1, called a skew number.

   So a Random-access list [RAList X] is one of
   - ra:empty
   - (kons Skew [Tree X] [RaList X])

   A Full binary tree [Tree X] is one of:
   - X
   - (node X [Tree X] [Tree X])
   (define-struct node (val left right))

   For each node n, 
      (= (tree-height (node-left n)) (tree-height (node-right n))).
   For each kons k, 
      (= (kons-size k) (tree-height (kons-tree k))).
   And for (make-kons s0 FT0 ... (make-kons sn FTn ra:empty)),
      1) For all 0 <= i < j <= n, si <= sj
      2) For all 0 <  i < j <= n, si <  sj

   In other words, only s0 and s1 maybe equal, with all other skew numbers 
   strictly increasing.

   The kons and ra:empty constructors should be thought of as just proper 
   list constructors (for lists of pairs nat, tree), but represented using 
   structures to allow for extending equal? and customizing printing.

   Algorithms
   ----------

   The elements of a list are given by a pre-order traversal of each tree in
   the order of the tree sequence.

   The usual list construction and deconstruction operations, {cons, first, 
   rest}, are O(1).  So all list operations that are O(f) using sequential 
   lists are O(f) using random access lists, trivially.

   However, index-based access and functional updates to elements, which are
   O(i) and O(n), respectively, using sequential lists, can be performed
   using random-access lists in O(min(i,lg n)), where n is the size of the 
   list and i is the index.

   To index and element, you find the appropriate tree then descend into the
   proper position in the tree.  Notice that each operation is O(lg n).
|#
(require racket/provide)
(require racket/unsafe/ops)

(define (zero? n) (eq? 0 n))

;; Provides `ra:*', dropping the prefix.
(provide (filtered-out (lambda (name)
                         (and (regexp-match?  "ra:" name)
                              (regexp-replace "ra:" name "")))
                       (all-defined-out)))

;; -------------------
;; Kons

(define (unsafe-kons-size k) (unsafe-struct-ref k 0))
(define (unsafe-kons-tree k) (unsafe-struct-ref k 1))
(define (unsafe-kons-rest k) (unsafe-struct-ref k 2))


(define-struct kons (size tree rest)
  #:property prop:equal+hash
  (list 
   (lambda (ra1 ra2 equal?)
     (and (= (unsafe-kons-size ra1) (unsafe-kons-size ra2))
          (tree-equal? (unsafe-kons-tree ra1) (unsafe-kons-tree ra2) equal?)
          (equal? (unsafe-kons-rest ra1) (unsafe-kons-rest ra2))))
   
   ;; I'm just guessing here...
   (lambda (ra equal-hash-code)
     (+ (bitwise-bit-field (+ (unsafe-kons-size ra) 
                              (equal-hash-code (unsafe-kons-tree ra))) 
                           0 14)
        (arithmetic-shift 
         (bitwise-bit-field (equal-hash-code (unsafe-kons-tree ra)) 0 14) 14)))
   
   (lambda (ra equal-hash-code)
     (+ (bitwise-bit-field (equal-hash-code (unsafe-kons-tree ra)) 14 28)
        (+ (unsafe-kons-size ra)
           (arithmetic-shift 
            (bitwise-bit-field (equal-hash-code (ra:rest ra)) 14 28) 14)))))
  
  #:property prop:custom-write
  ;; [RaListof X] Port Boolean -> Void  
  (lambda (ra p write?)
    (let ((print (if write? write display)))
      (let ((curly? (print-pair-curly-braces)))
        (display (if curly? "{" "(") p)
        
        (let loop ((ls ra))
          (cond [(ra:empty? ls) 'done]
                [(ra:cons? ls)
                 (print (ra:car ls) p)
                 (unless (ra:empty? (ra:cdr ls))
                   (display " " p)
                   (loop (ra:cdr ls)))]
                [else
                 (display ". " p)
                 (print ls p)]))
        
        (display (if curly? "}" ")") p))))
    
  #:property prop:sequence
  ;; [RaListof X] -> [Seq X]
  (lambda (ra)
    ;; Incurs logarithmic overhead at sequence construction time,
    ;; but keeps you from having to dispatch at each position.
    (let ((init (list->forest ra)))
      (make-do-sequence
       (lambda ()
         (values
          (lambda (x) (tree-val (car x)))
          (lambda (p)
            (let ((tr (car p)))
              (cond [(node? tr)
                     (cons (unsafe-node-left tr)
                           (cons (unsafe-node-right tr)
                                 (cdr p)))]
                    [else (cdr p)])))
          init cons? void void))))))


;; ------------------------
;; Full binary trees
;; See tests/tree for unit tests.

(define-struct node (val left right) #:prefab)

(define (unsafe-node-val n) (unsafe-struct-ref n 0))
(define (unsafe-node-left n) (unsafe-struct-ref n 1))
(define (unsafe-node-right n) (unsafe-struct-ref n 2))

(define (tree-equal? t1 t2 equal?)
  (if (node? t1)
      (and (node? t2)
           (equal? (unsafe-node-val t1) (unsafe-node-val t2))
           (tree-equal? (unsafe-node-left t1) (unsafe-node-left t2) equal?)
           (tree-equal? (unsafe-node-right t1) (unsafe-node-right t2) equal?))
      (equal? t1 t2)))

;; [Tree X] -> X
(define (tree-val t)
  (if (node? t)
      (unsafe-node-val t)
      t))

;; [X -> Y] [Tree X] -> [Tree Y]
(define (tree-map f t)
  (if (node? t)
      (node (f (unsafe-node-val t))
            (tree-map f (unsafe-node-left t))
            (tree-map f (unsafe-node-right t)))
      (f t)))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> [Tree R]
(define (tree-map/n f ts)
  (let recr ((ts ts))
    (match ts
      [(list (struct node (vs ls rs)) ...)
       (node (apply f vs) (recr ls) (recr rs))]
      [xs (apply f xs)])))

;; Nat [Tree X] Nat [X -> X] -> (values X [Tree X])
(define (tree-ref/update mid t i f)
  (cond [(zero? i)
         (if (node? t)
             (let ((x (unsafe-node-val t)))
               (values x
                       (node (f x)
                             (unsafe-node-left t)
                             (unsafe-node-right t))))
             (values t (f t)))]
        [(<= i mid)
         (let-values ([(v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (unsafe-node-left t) 
                                                (sub1 i) 
                                                f)])
           (values v* (node (unsafe-node-val t) t* (unsafe-node-right t))))]
        [else
         (let-values ([(v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (unsafe-node-right t) 
                                                (sub1 (- i mid)) 
                                                f)])
           (values v* (node (unsafe-node-val t) (unsafe-node-left t) t*)))]))

;; Special-cased above to avoid logarathmic amount of cons'ing 
;; and any multi-values overhead.  Operates in constant space.
;; [Tree X] Nat Nat -> X 
;; invariant: (= mid (half (sub1 (tree-count t)))) 
(define (tree-ref/a t i mid) 
  (cond [(zero? i) (tree-val t)] 
        [(<= i mid) 
         (tree-ref/a (unsafe-node-left t) 
                     (sub1 i) 
                     (half (sub1 mid)))] 
        [else 
         (tree-ref/a (unsafe-node-right t) 
                     (sub1 (- i mid)) 
                     (half (sub1 mid)))])) 

;; Nat [Tree X] Nat -> X
;; invariant: (= size (tree-count t))
(define (tree-ref size t i)
  (if (zero? i)
      (tree-val t)
      (tree-ref/a t i (half (sub1 size)))))

;; Nat [Tree X] Nat [X -> X] -> [Tree X]
(define (tree-update size t i f)
  (let recr ((mid (half (sub1 size))) (t t) (i i))
    (cond [(zero? i)
           (if (node? t)
               (node (f (unsafe-node-val t))
                     (unsafe-node-left t)
                     (unsafe-node-right t))
               (f t))]
          [(<= i mid)
           (node (node-val t) 
                 (recr (half (sub1 mid))
                       (node-left t) 
                       (sub1 i)) 
                 (node-right t))]
          [else
           (node (node-val t) 
                 (node-left t) 
                 (recr (half (sub1 mid))
                       (node-right t) 
                       (sub1 (- i mid))))])))

(define (tree-last t)
  (if (node? t)
      (tree-last (unsafe-node-right t))
      t))
  
;; ------------------------
;; Random access lists

;; See tests/ra-list for tests.

(define indx-msg "index ~a too large for: ~a")

;; [RaListof X]
(define ra:empty empty)

;; [Any -> Boolean]
(define ra:cons? kons?)

;; [Any -> Boolean]
(define ra:empty? empty?)

;; [Any -> Boolean]
;; Is x a PROPER list?
(define (ra:list? x)
  (or (ra:empty? x)
      (and (ra:cons? x)
           (ra:list? (unsafe-kons-rest x)))))

;; X [RaListof X] -> [RaListof X]  /\
;; X Y -> [RaPair X Y]
(define (ra:cons x ls)
  (match ls 
    [(struct kons (s t1 (struct kons (s t2 r))))
     (make-kons (add1 (double s)) (node x t1 t2) r)]
    [else 
     (make-kons 1 x ls)]))

(define (get-car+cdr name p)
  (match p
    [(struct kons (s (struct node (x t1 t2)) r))
     (let ((s* (half s)))
       (values x (make-kons s* t1 (make-kons s* t2 r))))]
    [(struct kons (s x r))
     (values x r)]
    [else
     (error name "expected cons, given: ~a" p)]))

(define (get-first+rest name p)
  (get-car+cdr name p))

;; [RaPair X Y] -> (values X Y)
(define (ra:car+cdr p)
  (get-car+cdr 'ra:car+cdr p))

;; [RaListof X] -> (values X [RaListof X])
(define (ra:first+rest ls)
  (get-first+rest 'ra:first+rest ls))

;; [RaListof X] -> X
(define (ra:first ls) 
  (let-values ([(f r) (get-first+rest 'ra:first ls)]) 
    f))

;; [RaListof X] -> [RaListof X]
(define (ra:rest ls)  
  (let-values ([(f r) (get-first+rest 'ra:rest ls)]) 
    r))

;; [RaPair X Y] -> X
(define (ra:car p) 
  (let-values ([(x y) (get-car+cdr 'ra:car p)]) 
    x))

;; [RaPair X Y] -> Y
(define (ra:cdr p)
  (let-values ([(x y) (get-car+cdr 'ra:cdr p)])
    y))

;; Consumes n = 2^i-1 and produces 2^(i-1)-1.
;; Nat -> Nat
(define-syntax-rule (half n)
  (arithmetic-shift n -1))
  
;; Nat -> Nat
(define (double n)
  (arithmetic-shift n 1))

;; [RaListof X] Nat [X -> X] -> (values X [RaListof X])
(define (ra:list-ref/update ls i f)
  (let recr ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s) 
          (let-values ([(v* t*) 
                        (tree-ref/update (half (sub1 s)) 
                                         (unsafe-kons-tree xs) 
                                         j 
                                         f)])
            (values v* (make-kons s t* (unsafe-kons-rest xs))))
          (let-values ([(v* r*) (recr (unsafe-kons-rest xs) (- j s))])
            (values v* (make-kons s (unsafe-kons-tree xs) r*)))))))

;; [RaListof X] Nat [X -> X] -> [RaListof X]
(define (ra:list-update ls i f)
  (let recr ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s)
          (make-kons s 
                     (tree-update s (unsafe-kons-tree xs) j f) 
                     (unsafe-kons-rest xs))
          (make-kons s 
                     (unsafe-kons-tree xs)
                     (recr (unsafe-kons-rest xs) 
                           (- j s)))))))

;; [RaListof X] Nat -> X
;; Special-cased above to avoid logarathmic amount of cons'ing 
;; and any multi-values overhead.  Operates in constant space.
(define (ra:list-ref ls i)
  (let loop ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s)
          (tree-ref s (unsafe-kons-tree xs) j)
          (loop (unsafe-kons-rest xs) (- j s))))))

;; [RaListof X] Nat X -> (values X [RaListof X])
(define (ra:list-ref/set ls i v)
  (ra:list-ref/update ls i (lambda (_) v)))
  
;; [RaListof X] Nat X -> [RaListof X]
(define (ra:list-set ls i v)
  ;; Nat [Tree X] Nat -> [Tree X]
  (define (tree-set mid t i)
    (cond [(zero? i)
           (if (node? t)
               (node v
                     (unsafe-node-left t)
                     (unsafe-node-right t))
               v)]
          [(<= i mid)
           (node (unsafe-node-val t)
                 (tree-set (half (sub1 mid))
                           (unsafe-node-left t)
                           (sub1 i))
                 (unsafe-node-right t))]
          [else
           (node (unsafe-node-val t)
                 (unsafe-node-left t)
                 (tree-set (half (sub1 mid))
                           (unsafe-node-right t)
                           (- (sub1 i) mid)))]))
  
  (let recr ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s) 
          (kons s 
                (tree-set (half (sub1 s)) (unsafe-kons-tree xs) j)
                (unsafe-kons-rest xs))
          (kons s (unsafe-kons-tree xs)
                (recr (unsafe-kons-rest xs) (- j s)))))))

;; [X Y -> Y] Y [RaListof X] -> Y
(define (ra:foldl/1 f a ls)
  (for/fold ([a a]) ([x (ra:in-list ls)])
    (f x a)))

;; [X Y -> Y] Y [RaListof X] -> Y
(define (ra:foldr/1 f b ls)
  (let recr ((ls ls))
    (cond [(ra:empty? ls) b]
          [else (let-values ([(fst rst) (ra:first+rest ls)])
                  (f fst (recr rst)))])))
     
;; [X Y ... Z -> Z] Z [RaListof X] [RaListof Y] ... -> Z
(define ra:foldl
  (case-lambda
    [(f a ls) (ra:foldl/1 f a ls)]
    [(f a . lss)
     (let loop ((lss lss) (a a))
       (match lss
         [(cons (match:ra:list) _) a]
         [(list (match:ra:cons xs rs) ...)
          (loop rs
                (apply f (append xs (list a))))]))]))

;; [X Y ... Z -> Z] Z [RaListof X] [RaListof Y] ... -> Z
(define ra:foldr
  (case-lambda
    [(f b ls) (ra:foldr/1 f b ls)]
    [(f b . lss)
     (let recr ((lss lss))
       (match lss
         [(cons (match:ra:list) _) b]
         [(list (match:ra:cons xs rs) ...)
          (apply f (append xs (list (recr rs))))]))]))

;; [X Y ... -> Any] [RaListof X] [RaListof Y] ... -> Any
(define ra:andmap
  (case-lambda
    [(f ls) (for/and ([x (ra:in-list ls)]) (f x))]
    [(f . lss)
     (match lss
       [(cons (match:ra:list) _) true]
       [else 
        (let loop ((lss lss))
          (match lss
            [(list (match:ra:cons xs (match:ra:list)) ...)
             (apply f xs)]
            [(list (match:ra:cons xs rs) ...)
             (and (apply f xs)
                  (loop rs))]))])]))

;; [X Y ... -> Any] [RaListof X] [RaListof Y] ... -> Any
(define ra:ormap
  (case-lambda
    [(f ls) (for/or ([x (ra:in-list ls)]) (f x))]
    [(f . lss)
     (match lss
       [(cons (match:ra:list) _) false]
       [else 
        (let loop ((lss lss))
          (match lss
            [(list (match:ra:cons xs (match:ra:list)) ...)
             (apply f xs)]
            [(list (match:ra:cons xs rs) ...)
             (or (apply f xs)
                 (loop rs))]))])]))
      
;; X ... -> [RaListof X]
(define (ra:list . xs)
  (foldr ra:cons ra:empty xs))

;; X ... [RaListof X] -> [RaListof X]
(define (ra:list* x . r+t)
  (let loop ((xs+t (cons x r+t)))
    (match xs+t
      [(list t) t]
      [(cons x xs+t) 
       (ra:cons x (loop xs+t))])))

;; Nat [Nat -> X] -> [RaListof X]
;; Optimized based on skew decomposition.
(define (ra:build-list n f)
  (define (build-tree/offset i n) ;; i = 2^j-1
    (let rec ((i i) (o n))
      (cond [(eq? 1 i) (f o)]
            [else
             (let ((i/2 (half i)))
               (node (f o)
                     (rec i/2 (add1 o))
                     (rec i/2 (add1 (+ o i/2)))))])))  
   (let loop ((n n) (a ra:empty))
     (if (zero? n) 
         a
         (let ((t (largest-skew-binary n)))
           (let ((n* (- n t)))
             (loop n*
                   (kons t 
                         (build-tree/offset t n*)
                         a)))))))

;; Nat X -> [RaListof X]
(define (ra:make-list n x)
  ;; Nat X -> [Tree X]
  (define (make-tree i) ;; i = 2^j-1
    (if (eq? 1 i) 
        x
        (let ((n (make-tree (half i))))
          (node x n n))))
  (let loop ((n n) (a ra:empty))
    (cond [(zero? n) a]
          [else 
           (let ((t (largest-skew-binary n)))
             (loop (- n t)
                   (make-kons t (make-tree t) a)))])))

;; A Skew is a Nat 2^k-1 with k > 0.

;; Skew -> Skew
(define (skew-succ t) (add1 (double t )))

;; Computes the largest skew binary term t <= n.
;; Nat -> Skew
(define (largest-skew-binary n)
  (if (eq? 1 n) 
      1
      (let* ((t (largest-skew-binary (half n)))
             (s (skew-succ t)))
        (if (> s n) t s))))

;; [X -> y] [RaListof X] -> [RaListof Y]
;; Takes advantage of the fact that map produces a list of equal size.
(define ra:map
  (case-lambda 
    [(f ls)
     (let recr ((ls ls))   
       (match ls
         [(struct kons (s t r))
          (make-kons s (tree-map f t) (recr r))]
         [else ra:empty]))]
    [(f . lss)
     (let recr ((lss lss))
       (cond [(ra:empty? (car lss)) ra:empty]
             [else
              ;; IMPROVE ME: make one pass over lss.
              (make-kons (kons-size (car lss))
                         (tree-map/n f (map kons-tree lss))
                         (recr (map kons-rest lss)))]))]))

;; Any -> Nat
(define (ra:count ls)
  (let recr ((ls ls))
    (if (kons? ls)
        (+ (unsafe-kons-size ls)
           (recr (unsafe-kons-rest ls)))
        0)))

;; [RaListof X] -> Nat
(define ra:length ra:count)

;; [RaListof X] -> X
(define (ra:second ls)  (ra:list-ref ls 1))
(define (ra:third ls)   (ra:list-ref ls 2))
(define (ra:fourth ls)  (ra:list-ref ls 3))
(define (ra:fifth ls)   (ra:list-ref ls 4))
(define (ra:sixth ls)   (ra:list-ref ls 5))
(define (ra:seventh ls) (ra:list-ref ls 6))
(define (ra:eighth ls)  (ra:list-ref ls 7))
(define (ra:ninth ls)   (ra:list-ref ls 8))
(define (ra:tenth ls)   (ra:list-ref ls 9))
(define (ra:last ls)
  (let loop ((ls ls))
    (let ((r (kons-rest ls)))
      (if (kons? r)
          (loop r)
          (tree-last (kons-tree ls))))))

;; [RaListof X] Nat -> [RaListof X]
(define (ra:list-tail ls i)
  (let loop ((xs ls) (j i))
    (cond [(zero? j) xs]
          [else (loop (ra:cdr xs) (sub1 j))])))

;; [RaListof X] ... -> [RaListof X]
(define (ra:append . lss)
  (cond [(empty? lss) ra:empty]
        [lss (let recr ((lss lss))
               (cond [(empty? (cdr lss)) (car lss)]
                     [else (ra:foldr/1 ra:cons
                                       (recr (cdr lss))
                                       (car lss))]))]))

;; [RaListof X] -> [RaListof X]
(define (ra:reverse ls)
  (ra:foldl/1 ra:cons ra:empty ls))

;; Not ready for release, but used internally.

;; Match patterns
;; --------------
(define-match-expander match:ra:list
  (syntax-rules ()
    [(match:ra:list) (quote ())]
    [(match:ra:list x y ...)
     (match:ra:cons x (match:ra:list y ...))])
  (syntax-rules ()
    [(_) ra:list]))

(define-match-expander match:ra:cons
  (syntax-rules (match:ra:list)
    ;; Specialized case for matching last element.
    [(match:ra:cons fst (match:ra:list))
     (struct kons (1 fst (match:ra:list)))]
    ;; General case.
    [(match:ra:cons fst rst)
     ;; IMPROVE ME
     (or (and (struct kons (_ (struct node (fst _ _)) _))
              (app ra:rest rst))
         (struct kons (_ fst rst)))])
  (syntax-rules ()
    [(_) ra:cons]))


;; Sequence syntax
;; ---------------

;; Produce a forest of all the trees in the given ralist.
;; (kons s1 t1 ... (kons sn tn ra:empty)) =>
;;   (cons t1 ... (cons tn empty))

;; [RaListof X] -> [Listof [Tree X]]
(define (list->forest ra)
  (cond [(ra:empty? ra) empty]
        [else (cons (kons-tree ra)
                    (list->forest (kons-rest ra)))]))

;; RaList -> (values val node forest)
;; Initialize sequence.
(define (sequence-init ra)
  (match ra
    [(quote ())
     (values false false false)]
    [(struct kons 
             (size 
              (struct node (x (and (struct node _) left-node) right-node)) 
              rest))
     (values x left-node (cons right-node (list->forest rest)))]
    [(struct kons
             (size (struct node (x left-leaf right-leaf)) rest))
     (values x false (cons left-leaf (cons right-leaf (list->forest rest))))]
    [(struct kons (size leaf (quote ())))
     (values leaf false empty)]
    
    [(struct kons (size leaf (struct kons (_ (and (struct node _) n) rest))))
     (values leaf n (list->forest rest))]
    
    [(struct kons (size leaf rest))
     (values leaf false (list->forest rest))]))

;; [U Node false] [U Forest false] -> X [U Node false] [U Forest false]
;; Takes one step in sequence iteration
(define (advance n f)
  (if n
      (let ((l (node-left n)))
        (cond [(node? l)
               (values (node-val n) l (cons (node-right n) f))]
              [else
               (values (node-val n) false (cons l (cons (node-right n) f)))]))
      (match f
        [(quote ()) (values false false false)]
        [(cons (and (struct node _) n) f)
         (match n
           [(struct node (x (and (struct node _) left) right))
            (values x left (cons right f))]
           [(struct node (x leaf-left leaf-right))
            (values x false (cons leaf-left (cons leaf-right f)))])]
        [(cons leaf f)
         (values leaf false f)])))

(define-sequence-syntax ra:in-list
  (lambda () #'(lambda (x) x))
  (lambda (stx)
    (syntax-case stx ()
      [((id) (_ ra-list-exp))
       #'[(id)
          (:do-in
           ([(v t f) (sequence-init ra-list-exp)])
           'outer-check
           ;; loop bindings
           ([v v] [t t] [f f])
           ;; pos check
           f
           ;; inner bindings
           ([(id) v]
            [(next tree forest) (advance t f)])
           
           #t ;; pre guard
           #t ;; post guard
           ;; loop args
           (next tree forest))]])))

;; This is a brain-dead for/list, but I don't see how doing anything
;; more complicated improves much over this simple code.
(define-syntax ra:for/list
  (syntax-rules ()
    [(ra:for/list . x)
     (foldr ra:cons ra:empty (for/list . x))]))
