#lang racket
(require "../ralist.rkt"
         (prefix-in mz: (only-in racket length)))

(provide in-list for/list)
(provide count=/c count>/c is-true/c arity-includes/c)

;; Any -> Flat-Contract
;; The input is acceptable whenever e is true.
;; Syntax for better error reporting -- quotes expression.
(define-syntax is-true/c
  (syntax-id-rules ()
    [(is-true/c e) 
     (flat-named-contract 
      (list 'is-true/c 'e)
      (let ((v e))
        (lambda (_) v)))]
    [is-true/c
     (lambda (v)
       (flat-named-contract
        (list 'is-true/c v)
        (lambda (_) v)))]))    
  
;; Nat -> Flat-Contract
;; The input must have a count equal to n.
(define (count=/c n)
  (flat-named-contract
   (list 'count=/c n)
   (lambda (x)
     (= n (count x)))))

;; Nat -> Flat-Contract
;; The input must have a count larger than n.
(define (count>/c n)
  (flat-named-contract
   (list 'count>/c n)
   (lambda (x)
     (> (count x) n))))

;; Nat -> Flat-Contract
;; The input must be a procedure that can be applied to n arguments.
(define (arity-includes/c n)
  (flat-named-contract
   (list 'arity-includes/c n)
   (lambda (f)
     (and (procedure? f)
          (procedure-arity-includes? f n)))))

;; Renamed for better error messages ("ra:cons?" instead of "kons?").
(define cons/c (procedure-rename cons? 'ra:cons?)) 

;; The range of all function contracts is `any' to avoid overhead.
(provide/contract 
 [cons       (-> any/c any/c any)]
 [empty      empty?]
 [first+rest (-> (and/c cons/c list?) any)]
 [first      (-> (and/c cons/c list?) any)]
 [rest       (-> (and/c cons/c list?) any)]
 [car+cdr    (-> cons/c any)]
 [car        (-> cons/c any)]
 [cdr        (-> cons/c any)]
 [cons?      (-> any/c any)]
 [empty?     (-> any/c any)]
 [list?      (-> any/c any)]
 [list       (->* () () #:rest (listof any/c) any)]
 [list*      (->* (any/c) () #:rest (listof any/c) any)]
 [count      (-> any/c any)]
 [length     (-> list? any)]
 [append     (->* () () #:rest (listof list?) any)] ;; HANDLE IMPROPER CASE
 [reverse    (-> list? any)]
 [second     (-> (and/c list? (count>/c 1)) any)] 
 [third      (-> (and/c list? (count>/c 2)) any)]
 [fourth     (-> (and/c list? (count>/c 3)) any)]
 [fifth      (-> (and/c list? (count>/c 4)) any)]
 [sixth      (-> (and/c list? (count>/c 5)) any)]
 [seventh    (-> (and/c list? (count>/c 6)) any)]
 [eighth     (-> (and/c list? (count>/c 7)) any)]
 [ninth      (-> (and/c list? (count>/c 8)) any)]
 [tenth      (-> (and/c list? (count>/c 9)) any)]
 [last       (-> (and/c cons/c list?) any)]
 
 [list-tail  (->d ([xs (count>/c (sub1 i))]
                   [i natural-number/c])
                  ()
                  any)] 
 
 [make-list  (-> natural-number/c any/c any)]
 
 [list-ref   (->d ([ls (count>/c i)]
                   [i natural-number/c])
                  ()
                  any)]
 
 [list-set   (->d ([ls (count>/c i)]
                   [i natural-number/c]
                   [x any/c])
                  ()
                  any)]
 
 [list-update 
  (->d ([ls (count>/c i)]
        [i natural-number/c]
        [f (arity-includes/c 1)])
       ()
       any)]
 
 [list-ref/update 
  (->d ([ls (count>/c i)]
        [i natural-number/c]
        [f (arity-includes/c 1)])
       ()
       any)]
 
 [list-ref/set
  (->d ([ls (count>/c i)]
        [i natural-number/c]
        [x any/c])
       ()
       any)]
 
 [build-list 
  (->d ([n natural-number/c]
        [f (or/c (is-true/c (zero? n))
                 (arity-includes/c 1))])
       ()
       any)]
 
 [map 
  (->d ([f (or/c (is-true/c (zero? (count xs)))
                 (arity-includes/c (add1 (mz:length ...))))]
        [xs list?])
       ()
       #:rest ... 
       (listof (and/c list? (count=/c (count xs))))
       any)]
 
 
 [andmap
  (->d ([f (or/c (is-true/c (zero? (count xs)))
                 (arity-includes/c (add1 (mz:length ...))))]
        [xs list?])
       ()
       #:rest ... 
       (listof (and/c list? (count=/c (count xs))))
       any)]
 
 [ormap
  (->d ([f (or/c (is-true/c (zero? (count xs)))
                 (arity-includes/c (add1 (mz:length ...))))]
        [xs list?])
       ()
       #:rest ... 
       (listof (and/c list? (count=/c (count xs))))
       any)]
 
 [foldr     
  (->d ([f (or/c (is-true/c (zero? (count xs)))
                 (arity-includes/c (+ 2 (mz:length ...))))]
        [b any/c]
        [xs list?])
       ()
       #:rest ... 
       (listof (and/c list? (count=/c (count xs))))
       any)]

 [foldl     
  (->d ([f (or/c (is-true/c (zero? (count xs)))
                 (arity-includes/c (+ 2 (mz:length ...))))]
        [a any/c]
        [xs list?])
       ()
       #:rest ... 
       (listof (and/c list? (count=/c (count xs))))
       any)])