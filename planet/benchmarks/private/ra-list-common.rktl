;; Benchmarks of operations against list counterpart.
;; Any (-> Void) ... -> Void
(define (do str . ts)
  (printf "~a~n" str)  
  (for-each (λ (t p) 
              (printf "~a: " p)
              (collect-garbage)
              (void (time (t))))
            ts
            ps)
  (newline))

(define (do* str run procs)
  (apply do str (map (λ (p) (λ () (run p))) procs)))
    

(printf "~a~n" title)
(printf "========================~n")
(display module-path) (newline)
(newline)
(display '(define ls1 (build-list 1000000 (lambda (i) i)))) (newline)
(display '(define ls2 (build-list 1000000 (lambda (i) i)))) (newline)
(newline)

(let ()
  (define ra:ls1 (ra:build-list 1000000 (lambda (i) i)))
  (define ra:ls2 (ra:build-list 1000000 (lambda (i) i)))
  (define ls:ls1 (ls:build-list 1000000 (lambda (i) i)))
  (define ls:ls2 (ls:build-list 1000000 (lambda (i) i)))
  
  (let ((i 1000000))
    (do* `(for ((j (in-range 10)))
            (build-list ,i (lambda (i) i)))
         (λ (build-list)
           (for ((j (in-range 10))) (build-list i (lambda (i) i))))
         (list ra:build-list ls:build-list #;tr:build-list)))
  
  (let ((i 10000000))
    (do* `(build-list ,i (lambda (i) i))
         (λ (build-list)
           (build-list i (lambda (i) i)))
         (list ra:build-list ls:build-list #;tr:build-list)))
  
  (let ((i 1000000))
    (do `(for ((j (in-range 10)))
           (make-list ,i 'x))
      (lambda () (for ((j (in-range 10))) (ra:make-list i 'x)))
      (lambda () (for ((j (in-range 10))) (ls:make-list i 'x)))))
  
  (let ((i 10000000))
    (do `(make-list ,i 'x)
      (lambda () (ra:make-list i 'x))
      (lambda () (ls:make-list i 'x))))
  
  (do '(equal? ls1 ls2)
    (λ () (equal? ra:ls1 ra:ls2))
    (λ () (equal? ls:ls1 ls:ls2)))
  
  (do '(length ls)
    (λ () (ra:length ra:ls1))
    (λ () (ls:length ls:ls1)))
  
  (let ((i 10))
    (do `(for ((i (in-range ,i))) (map add1 ls))
      (λ () (for ((i (in-range i))) (ra:map add1 ra:ls1)))
      (λ () (for ((i (in-range i))) (ls:map add1 ls:ls1)))))
  
  (do '(foldl + 0 ls)
    (λ () (ra:foldl + 0 ra:ls1))
    (λ () (ls:foldl + 0 ls:ls1)))
  
  (do '(foldr + 0 ls)
    (λ () (ra:foldr + 0 ra:ls1))
    (λ () (ls:foldr + 0 ls:ls1)))
  
  (do '(for ((i (in-list ls))) i)
    (λ () (for ((i (ra:in-list ra:ls1))) i))
    (λ () (for ((i (ls:in-list ls:ls1))) i)))
  
  (let ((i 100000))
    (do `(for ((i (in-range ,i)))
           (list-ref ls 0))
      (λ () (for ((i (in-range i)))
              (ra:list-ref ra:ls1 0)))
      (λ () (for ((i (in-range i)))
              (ls:list-ref ls:ls1 0)))))
  
  (let ((i 100000))
    (do `(for ((i (in-range ,i)))
           (tenth ls))
      (λ () (for ((i (in-range i)))
              (ra:tenth ra:ls1)))
      (λ () (for ((i (in-range i)))
              (ls:tenth ls:ls1)))))
  
  (let ((i 1000))
    (do `(for ((i (in-range ,i)))
           (list-ref ls 999999))
      (λ () (for ((i (in-range i)))
              (ra:list-ref ra:ls1 999999)))
      (λ () (for ((i (in-range i)))
              (ls:list-ref ls:ls1 999999)))))
  
  (let ((i 1000))
    (do `(for ((i (in-range ,i)))
           (last ls))
      (λ () (for ((i (in-range i)))
              (ra:last ra:ls1)))
      (λ () (for ((i (in-range i)))
              (ls:last ls:ls1))))))

