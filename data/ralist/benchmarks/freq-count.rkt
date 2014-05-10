#lang racket
(provide run-freq-count-benchmark)

;; Benchmark: purely functional frequency counting function.
(require (prefix-in ra: "../../ralist.rkt"))

(define (cnt-ra.0 f)
  (cnt-fp
   f
   (lambda () (ra:make-list (- HIGH LOW) 0))
   (lambda (ls freq)
     (ra:list-update ls (- freq LOW) add1))
   (lambda (ls)
     (out (lambda () (for ((v (ra:in-list ls))
                           (i (in-range (- HIGH LOW))))
                       (unless (= v 0) (printf F (+ 80 i) v))))))))

(define (cnt-ra.1 f)
  (cnt-fp
   f
   (lambda () (ra:make-list (- HIGH LOW) 0))
   (lambda (ls freq)
     (ra:list-update ls (- freq LOW) add1))
   (lambda (ls)
     (out (lambda () (for ((i (in-range (- HIGH LOW))))
                       (let ((v (ra:list-ref ls i)))
                         (unless (= v 0) (printf F (+ 80 i) v)))))))))



(define HIGH 22000)
(define LOW     80)
;; Frequency is in [LOW ... HIGH)

(define SIZE 1000000) ;; sample size for file

(define F "~s: ~s\n") ;; output format for result lines

;; Nat -> String
;; generate file f with n frequencies between 80 and 22000
(define (gen n)
  (define (freq) (+ LOW (random (- HIGH LOW))))
  (with-output-to-string 
   (lambda () (for ((i (in-range n))) (printf "~s " (freq))))))

;; Nat -> Void
(define (experiment SIZE)
  ;; -----------------------------------------------------------------------------
  ;; creating the sample
  (define output (gen SIZE))

  (define (test str f)
    (collect-garbage)
    (printf "~a @ ~a " SIZE str)
    (time (f output))
    (void))

  (test "vector:" cnt-vec)
  (test "a list:" cnt-alst)
  (test "bst   :" cnt-BST )
  (test "hash  :" cnt-ht)
  (test "ra.0  :" cnt-ra.0)
  (test "ra.1  :" cnt-ra.1)
  (newline))

;; -----------------------------------------------------------------------------
;; functions for gathering statistics
;; read file f and count how many times each frequency occurs in a vector
;; write result to file g in ascending order of frequencies

;; String -> String
;; imperative vector update via [Vectorof Nat]
(define (cnt-vec f)
  (define a (make-vector (- HIGH LOW)))
  (define (up freq)
    (define i (- freq LOW))
    (vector-set! a i (+ (vector-ref a i) 1)))
  (with-input-from-string f
    (letrec ((loop
      (lambda ()
        (define nxt (read))
        (unless (eof-object? nxt) (up nxt) (loop)))))
      loop))
  ;; ---
  (with-output-to-string
    (lambda ()
      (for ((i (in-range (- HIGH LOW))))
           (define v (vector-ref a i))
           (unless (= v 0) (printf F (+ 80 i) v))))))

;; Fixed bug discussed here:
;; http://list.cs.brown.edu/pipermail/plt-scheme/2009-April/032608.html

;; String -> String
;; sort, then create association list via [Listof [List Nat Nat]]
(define (cnt-alst f)
  (define l:in
    (with-input-from-string f
      (letrec ((L
        (lambda ()
          (define nxt (read))
          (if (eof-object? nxt) '() (cons nxt (L))))))
        L)))
  (define l:st (sort l:in <))
  (define res
    (let L ([l (cdr l:st)][p (car l:st)][c 1])
      (if (null? l) (list (list p c))
          (let ([a (car l)])
            (if (= a p)
                (L (cdr l) p (+ c 1))
                (cons (list p c) (L (cdr l) (car l) 1)))))))
  (out-al res))

;; String String (-> Assoc) (Assoc Nat -> Assoc) (Assoc -> Void) -> Void
;; create association list on the fly
(define (cnt-fp f nu up out)
  (out (with-input-from-string f
         (lambda ()
           (let L ([a (nu)])
             (define nxt (read))
             (if (eof-object? nxt) a (L (up a nxt))))))))

(define (cnt-AL f)
  (cnt-fp f
          (lambda () '())
          (lambda (al freq)
            (let L ((al al))
              (if (null? al)
                  (list (list freq 1))
                  (let* ([a (car al)]
                         [key (car a)])
                    (if (= key freq)
                        (cons (list key (+ (cadr a) 1)) (cdr al))
                        (cons a (L (cdr al))))))))
          out-al))

(define (cnt-BST f)
  (define-struct node (lft info count rgt))
  ;; A BST is one of:
  ;; -- '()
  ;; -- (make-node BST Frequency Nat BST)
  (cnt-fp f
          (lambda () '())
          (lambda (a freq)
            (let L ([bst a])
              (if (null? bst)
                  (make-node '() freq 1 '())
                  (let* ([a (node-info bst)]
                         [lft (node-lft bst)]
                         [rgt (node-rgt bst)]
                         [cnt (node-count bst)])
                    (cond
                      [(< freq a) (make-node (L lft) a cnt rgt)]
                      [(= freq a) (make-node lft a (+ cnt 1) rgt)]
                      [else (make-node lft a cnt (L rgt))])))))
          (lambda (a)
            (out (lambda ()
                   (let L ((bst a))
                     (unless (null? bst)
                       (let* ([a (node-info bst)]
                              [lft (node-lft bst)]
                              [rgt (node-rgt bst)]
                              [cnt (node-count bst)])
                         (if (and (null? lft) (null? rgt))
                             (printf F a cnt)
                             (begin
                               (L lft)
                               (printf F a cnt)
                               (L rgt)))))))))))

(define (cnt-ht f)
  (cnt-fp
   f
   (lambda () #hash())
   (lambda (ht freq)
     (hash-update ht freq add1 0))
   (lambda (H)
     (out (lambda ()
            (for ((i (in-range LOW HIGH)))
              (define v (hash-ref H i 0))
              (unless (zero? v)
                (printf "~s: ~s\n" i v)))))
     ;; BUG: does not write in specified order since hash-for-each iterates
     ;; in an unspecified order.
     #;
     (out (lambda () (hash-for-each H (lambda (k v) (printf "~s: ~s\n" k v))))))))


;; -----------------------------------------------------------------------------
;; auxiliaries

;; String (-> Void) -> Void
;; create file f from th
(define (out th) (with-output-to-string th))

;; [Listof (List X Y)] -> String
(define (out-al res)
  (out (lambda () (for-each (lambda (i)  (printf F (car i) (cadr i))) res))))

;; -----------------------------------------------------------------------------
(define (run-freq-count-benchmark)
  (printf "Frequency counting benchmark~n")
  (printf "============================~n")
  (printf "http://lists.racket-lang.org/users/archive/2009-April/032214.html~n")
  (printf "Rewritten to use string ports in place of file IO.~n")
  (newline)
  (let L ((i 1000)) (unless (> i SIZE) (experiment i) (L (* 10 i)))))
