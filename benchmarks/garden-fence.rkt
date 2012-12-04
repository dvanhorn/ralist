#lang racket
;; Garden fence encryption benchmark
;; http://list.cs.brown.edu/pipermail/plt-scheme/2009-March/031313.html

;; All tests have been moved to tests/garden-fence.
(provide run-garden-fence-benchmark
         (rename-out [encrypt-ra encrypt]
                     [decrypt-ra decrypt])
         (struct-out crypt)
         crypts)

;; ----------------------------------------------------
;; Used in DR

(define (make-list n x)
  (build-list n (lambda (i) x)))

;; ----------------------------------------------------
;; Shared between ra and lr

;; app-rev is (compose append reverse)
(define (app-rev sl ls)
  (cond [(empty? sl) ls]
        [else (app-rev (cdr sl) (cons (car sl) ls))]))

;; ----------------------------------------------------
;; Shared between ve and ra

;; String [Listof Nat] -> String
;; Permute the string according to the given permutation.
(define (permute str perm)
   (permuter str perm
             (lambda (i j) (list i j))))

;; String [Listof Nat] -> String
;; Unpermute the string according to the given permutation.
(define (unpermute str perm)
   (permuter str perm
             (lambda (i j) (list j i))))

;; String [Listof Nat] [Nat Nat -> [List Nat Nat]] -> String
;; Abstraction of permute/unpermute.
(define (permuter str perm f)
   (let ([ans (string-copy str)])
     (let loop ([i 0] [p perm])
       (cond [(= i (string-length str)) ans]
             [else (string-set! ans
                                (car (f i (car p)))
                                (string-ref str
                                            (cadr (f i (car p)))))
                   (loop (add1 i)
                         (cdr p))]))))

;; ----------------------------------------------------
;; Imperative vector solution
;; http://list.cs.brown.edu/pipermail/plt-scheme/2009-March/031313.html

;; [Vectorof [Listof X]] Nat X -> Void
;; Set v[i] to (cons x v[i]).
(define (vector-cons! v i x)
   (vector-set! v i (cons x (vector-ref v i))))

;; Nat Nat -> [Listof Nat]
;; Generate a fence permutation of the given
;; height (> 1) for strings of length len.
(define (fence-ve height len)
   (let ([bot 0]
         [top (sub1 height)]
         [vec (make-vector height empty)])

     (let loop ([n 0] [level 0] [move add1])
       (cond [(= n len) (void)]
             [(< level bot) (loop n (add1 bot) add1)]
             [(> level top) (loop n (sub1 top) sub1)]
             [else
              (vector-cons! vec level n)
              (loop (add1 n) (move level) move)]))

     (apply append (map reverse (vector->list vec)))))

;; String Nat -> String
(define (encrypt-ve text height)
   (permute text (fence-ve height (string-length text))))

;; String Nat -> String
(define (decrypt-ve text height)
   (unpermute text (fence-ve height (string-length text))))

;; ----------------------------------------------------
;; Functional random access list solution

(require (prefix-in ra: "../main.rkt"))

;; Nat Nat -> [Listof Nat]
;; Generate a fence permutation of the given
;; height (> 1) for strings of length len.
(define (fence-ra height len)
   (let ([bot 0]
         [top (sub1 height)])
     (let loop ([n 0] [level 0] [move add1] [rls (ra:make-list height empty)])
       (cond [(= n len) 
              (ra:foldr app-rev empty rls)]
             [(< level bot) (loop n (add1 bot) add1 rls)]
             [(> level top) (loop n (sub1 top) sub1 rls)]
             [else
              (loop (add1 n) 
                    (move level) 
                    move
                    (ra:list-update rls level (lambda (ls) (cons n ls))))]))))

;; String Nat -> String
(define (encrypt-ra text height)
   (permute text (fence-ra height (string-length text))))

;; String Nat -> String
(define (decrypt-ra text height)
   (unpermute text (fence-ra height (string-length text))))


;; ----------------------------------------------------
;; Felleisen, combinator solution
;; http://list.cs.brown.edu/pipermail/plt-dev/2009-April/000532.html

;; String Nat -> String
;; encrypt according to fence shape
(define (encrypt-co ls n)
   (list->string (wave ls n)))

;; String Nat -> String
;; decrypt according to fence shape
(define (decrypt-co s n)
   (list->string
    (sort2 (wave (for/list ((i (in-naturals)) (c s)) i) n)
           (string->list s))))

;; [Listof X] Nat -> [Listof X]
;; create a wave from the list, depth n
;; [needed because in Racket, string != (Listof Char)]
(define (wave ls n)
   (sort2 (in-list (shared ((x (append (range 1 n) 
                                       (range (- n 1) 2)  x))) x))
          ls))

;; [Listof Nat] [Sequence Y] -> [Listof Y]
;; sort lst according to indicies in list inds
(define (sort2 ks ls)
   (map second (sort (for/list ((k ks) 
                                (l ls)) 
                       (list k l)) < #:key car)))

;; Nat Nat -> [Listof Nat]
(define (range lo hi)
   (if (>= hi lo)
       (build-list (+ (- hi lo) 1) (lambda (i) (+ lo i)))
       (build-list (+ (- lo hi) 1) (lambda (i) (- lo i)))))

;; ----------------------------------------------------
;; Tobin-Hochstadt, cycle solution
;; http://list.cs.brown.edu/pipermail/plt-dev/2009-April/000533.html

(define (rail n l)
  (zip-sort (for/list ([i (in-cycle (in-range 1 (add1 n)) 
                                    (in-range (sub1 n) 1 -1))]
                       [e l])
               (cons i e))))

(define (derail n s)
  (zip-sort (map cons 
                 (rail n (for/list ([i (in-naturals)] [e s]) i))
                 s)))

(define (zip-sort ks/vs)
  (map cdr (sort #:key car ks/vs <)))

(define (encrypt-cy s n) (apply string (rail n s)))
(define (decrypt-cy s n) (apply string (derail n (string->list s))))

;; ----------------------------------------------------
;; Felleisen, output data driven design recipe solution 
;; http://list.cs.brown.edu/pipermail/plt-scheme/2009-March/031344.html
;; Revised, based on personal communication, 05.02.2009

(define X '_)

#;
(check-expect (encrypt "diesisteinklartext" 6) "dkinleiasertittxse")

(define (encrypt-dr str h)
   (list->string (fence-dr (string->list str) h)))

;; [Listof X] -> [Listof X]
(define (fence-dr lox h)
  (local ((define a (apply append (transpose (waves lox h)))))
    (filter (lambda (e) (not (eq? X e))) a)))

;; [Listof X] Nat -> [Listof [Listof (U X Char)]]
;; chop the list into as many pieces of length h, plus padding of the 
;; last one
#;
(check-expect (waves '(d i e s i s t e i n k l a r t e x t) 6)
               '((d i e s i s) (_ n i e t _) (k l a r t e) (_ _ _ t x  _)))
#;
(check-expect (waves '(d i e s i) 3) '((d i e) (_ s _) (i _ _)))


(define (waves str h)
  (local ((define (down str)
            (cond
              [(>= h (length str)) (list (fill h str))]
              [else (cons (take str h) (up (drop str h)))]))
          (define (up str)
            (cond
              [(>= (- h 2) (length str)) (list (pad (fill (- h 2) str)))]
              [else (cons (pad (take str (- h 2))) (down (drop str (- h 2))))]))
          (define (pad str) (append (list X) (reverse str) (list X)))
          (define (fill h str) (append str (make-list (- h (length str)) X))))
    (down str)))

#;
(define (waves str h)
  (local ((define (down str)
            (cond
              [(>= h (length str)) (list (append str (fill h str)))]
              [else (cons (take str h) (up (drop str h)))]))
          (define (up str)
            (cond
              [(>= (- h 2) (length str))
               (list (append (fill (- h 1) str) (reverse (cons X  str))))]
              [else (cons (cons X (reverse (cons X (take str (- h  2)))))
                          (down (drop str (- h 2))))]))
          (define (fill h str) 
            (build-list (- h (length str)) (lambda (i) X))))
    (down str)))

;; [Listof [Listof X]] -> [Listof [Listof X]]
;; transpose the matrix
#;
(check-expect
  (transpose '((d i e s i s) (_ n i e t _) (k l a r t e) (_ _ _ t x _)))
  '((d _ k _) (i n l _) (e i a _) (s e r t) (i t t x) (s _ e _)))

(define (transpose m)
   (cond
     [(empty? (car m)) '()]
     [else (cons (map car m) (transpose (map cdr m)))]))

(define (decrypt-dr str h)
  (local ((define e (fence-dr (build-list (string-length str) (lambda (i) i)) h))
          (define x (map list e (string->list str)))
          (define y (sort x (lambda (i j) (<= (car i) (car j)))))
          (define z (map second y)))
    (list->string z)))

;; some demonstration code
;(define e (encrypt "diesisteinklartext" 6))
;(check-expect (decrypt e 6) "diesisteinklartext")


;; ----------------------------------------------------
;; Felleisen linear vector mutation
;; Personal communication, 05.02.2009

;; String Nat -> String
;; encrypt according to fence shape 
;(check-expect (encrypt-lv "diesisteinklartext" 6) "dkinleiasertittxse")
(define (encrypt-lv str h) (list->string (fence-lv str h)))

;; String Nat -> String 
;; decrypt according to fence shape 
;(check-expect (decrypt-lv  "dkinleiasertittxse" 6) "diesisteinklartext")
(define (decrypt-lv str h)
  (define LL (string-length str))
  (define wv (fence-lv (range 0 (- LL 1)) h))
  (define rs (make-string LL))
  (for ((i wv) (c str)) (string-set! rs i c))
  rs)

;; [Listof X] Nat -> [Listof X]
;; turn the list into waves of depth n and collect from top down
;; example:
;;                               1   5    9           
;; 1 2 3 4 5 6 7 8 9 10 11  ==>   2 4 6  8 10    ==> 1 5 9 2 4 6 8 10 3 7 11 
;;                                 3   7    11 

;(check-expect (fence-lv '(1 2 3 4 5 6) 3) '(1 5 2 4 6 3))
;(check-expect (fence-lv '(1 2 3 4 5 6 7 8 9 10 11) 3) '(1 5 9 2 4 6 8 10 3 7 11))

(define (fence-lv lx n)
  (define i (in-list (shared ((x (append (range 1 n) (range (- n 1) 2) x))) x)))
  (define vc (make-vector n '()))
  (for ((i i) (x lx)) (vector-set! vc (- i 1) (cons x (vector-ref vc (- i 1)))))
                   ;; (vector-cons! vc (- i 1) x)
  (apply append (map reverse (vector->list vc))))


;; The more efficient `build-list'-based version above is used instead.
#;
(define (range-lv L H)
  (for/list ((i (if (>= H L) (in-range L (+ H 1)) (in-range L (- H 1) -1)))) i))


;; ----------------------------------------------------

(define (encrypt-lr str h) (list->string (fence-lv str h)))

;; String Nat -> String 
;; decrypt according to fence shape 
;(check-expect (decrypt-lr  "dkinleiasertittxse" 6) "diesisteinklartext")
(define (decrypt-lr str h)
  (define LL (string-length str))
  (define wv (fence-lr (range 0 (- LL 1)) h))
  (define rs (make-string LL))
  (for ((i wv) (c str)) (string-set! rs i c))
  rs)

;; [Listof X] Nat -> [Listof X]
;; turn the list into waves of depth n and collect from top down
;; example:
;;                               1   5    9           
;; 1 2 3 4 5 6 7 8 9 10 11  ==>   2 4 6  8 10    ==> 1 5 9 2 4 6 8 10 3 7 11 
;;                                 3   7    11 

;(check-expect (fence-lr '(1 2 3 4 5 6) 3) '(1 5 2 4 6 3))
;(check-expect (fence-lr '(1 2 3 4 5 6 7 8 9 10 11) 3) '(1 5 9 2 4 6 8 10 3 7 11))

(define (fence-lr lx n)
  (define i (in-list (shared ((x (append (range 1 n) (range (- n 1) 2) x))) x)))
  (ra:foldr app-rev
            empty
            (for/fold ([ls (ra:make-list n '())])
              ((i i) (x lx))
              (ra:list-update ls (sub1 i) (lambda (ls) (cons x ls))))))



;; ----------------------------------------------------
;; Benchmark setup

(define-struct crypt (name en de))
(define crypts
  (list (make-crypt "ve" encrypt-ve decrypt-ve)
        (make-crypt "ra" encrypt-ra decrypt-ra)
        (make-crypt "dr" encrypt-dr decrypt-dr)
        (make-crypt "co" encrypt-co decrypt-co)
        (make-crypt "cy" encrypt-cy decrypt-cy)
        (make-crypt "lv" encrypt-lv decrypt-lv)
        (make-crypt "lr" encrypt-lr decrypt-lr)))

(define (do size crypts)
  (define str (build-string size (lambda (i) #\x)))
  
  (write `(define str (build-string ,size (lambda (i) #\x))))
  (newline)
  (newline)
  
  (display '(encrypt str 20))
  (newline)
  (for-each (lambda (c)
              (printf "~a: " (crypt-name c))
              (collect-garbage)
              (time (void ((crypt-en c) str 20))))
            crypts)
  
  (newline)
  
  (display '(decrypt str 20))
  (newline)
  (for-each (lambda (c)
              (printf "~a: " (crypt-name c))
              (collect-garbage)
              (time (void ((crypt-de c) str 20))))
            crypts)
  (newline))
  
(define header
#<<HEADER
Garden fence encryption benchmark
=================================
http://lists.racket-lang.org/users/archive/2009-March/031274.html

Key:

ve = Van Horn imperative vector
http://lists.racket-lang.org/users/archive/2009-March/031277.html

ra = random access list (translation of above)

dr = Felleisen output data driven design recipe
http://lists.racket-lang.org/users/archive/2009-March/031306.html
(Omitted from 1,000,000 chars case since it takes too long)

co = Felleisen combinator
http://lists.racket-lang.org/dev/archive/2009-April/000532.html

cy = Tobin-Hochstadt in-cycle
http://lists.racket-lang.org/dev/archive/2009-April/000533.html

lv = Felleisen linear vector mutation

lr = random access list (translation of above)

HEADER
)

(define (run-garden-fence-benchmark)
  (printf header)
  (do 10000 crypts)
  (do 100000 crypts)
  (do 1000000 (filter (lambda (c) (not (equal? "dr" (crypt-name c)))) crypts)))
