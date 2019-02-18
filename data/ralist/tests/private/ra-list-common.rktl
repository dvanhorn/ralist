(check-true (empty? empty))
(check-false (empty? (cons 9 empty)))
(check-false (empty? 'your-mom))

(check-true (cons? (cons 9 empty)))
(check-true (cons? (cons 1 2)))
(check-false (cons? empty))
(check-false (cons? 'your-mom))

(check-equal? (first (cons 9 empty)) 9)
(check-equal? (rest (cons 9 empty)) empty)

(check-equal? (car (cons 1 2)) 1)
(check-equal? (cdr (cons 1 2)) 2)

(check-fail (first+rest empty))
(check-fail (first empty))
(check-fail (rest empty))
(check-fail (car+cdr empty))
(check-fail (car empty))
(check-fail (cdr empty))

(check-equal? (second  (list 'a 'b)) 'b)
(check-equal? (third   (list 'a 'b 'c)) 'c)
(check-equal? (fourth  (list 'a 'b 'c 'd)) 'd)
(check-equal? (fifth   (list 'a 'b 'c 'd 'e)) 'e)
(check-equal? (sixth   (list 'a 'b 'c 'd 'e 'f)) 'f)
(check-equal? (seventh (list 'a 'b 'c 'd 'e 'f 'g)) 'g)
(check-equal? (eighth  (list 'a 'b 'c 'd 'e 'f 'g 'h)) 'h)
(check-equal? (ninth   (list 'a 'b 'c 'd 'e 'f 'g 'h 'i)) 'i)
(check-equal? (tenth   (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)) 'j)

(check-fail (first  (list* 1 2 3 4 5)))
(check-fail (rest   (list* 1 2 3 4 5)))
(check-fail (second (list* 1 2 3 4 5)))

(check-equal? (map add1 empty) empty)
(check-equal? (map add1 (cons 0 (cons 1 (cons 2 empty))))
              (cons 1 (cons 2 (cons 3 empty))))

(check-equal? (foldl + 0 empty) 0)
(check-equal? (foldl + 0 (cons 1 (cons 2 (cons 3 empty)))) 6)
(check-equal? (foldl cons empty empty) empty)
(check-equal? (foldl cons empty (cons 0 (cons 1 (cons 2 (cons 3 empty)))))
              (cons 3 (cons 2 (cons 1 (cons 0 empty)))))

(check-equal? (foldr + 0 empty) 0)
(check-equal? (foldr + 0 (cons 1 (cons 2 (cons 3 empty)))) 6)
(check-equal? (foldr cons empty empty) empty)
(check-equal? (foldr cons empty (cons 0 (cons 1 (cons 2 (cons 3 empty)))))
              (cons 0 (cons 1 (cons 2 (cons 3 empty)))))

(check-equal? (for/fold ([sum 0]) ([i empty]) (+ sum i)) 0)
(check-equal? (for/fold ([sum 0]) ([i (cons 1 (cons 2 (cons 3 empty)))]) 
                (+ sum i)) 6)
(check-equal? (for/fold ([str ""]) ([c (cons "a" (cons "b" (cons "c" empty)))]) 
                (string-append str c))
              "abc")

(check-true (list? empty))
(check-true (list? (cons 9 empty)))
(check-false (list? 'your-mom))

; Intentionally removed now that improper lists are supported. 1.11
;(check-exn exn? (λ () (cons 9 9)))

(check-equal? (list-ref (cons 9 empty) 0) 9)
(check-equal? (list-set (cons 9 empty) 0 4)
              (cons 4 empty))
(check-equal? (list-ref (cons 1 2) 0) 1)

(check-equal? (list-ref (list 'a 'b 'c 'd 'e) 2) 'c)
(check-equal? (list-set (list 'a 'b 'c 'd 'e) 2 'f)
              (list 'a 'b 'f 'd 'e))

(check-equal? (list-ref (list* 'a 'b 'c 'd 'e) 2) 'c)
(check-equal? (list-set (list* 'a 'b 'c 'd 'e) 2 'f)
              (list* 'a 'b 'f 'd 'e))

;; Example from docs
(check-equal? (list-set (list 'x 'y 'z) 2 'c)
              (list 'x 'y 'c))

(let ((ls (build-list 100 (lambda (i) i))))
  (for/list ([i (in-range 100)])
    (check-equal? (list-ref ls i) i)))

(let-values ([(v l) (list-ref/set (list 'a 'b 'c 'd 'e) 2 'f)])
  (check-equal? v 'c)
  (check-equal? l (list 'a 'b 'f 'd 'e)))

(check-equal? (list-update (list 5 6 7 8 9) 0 add1)
              (list 6 6 7 8 9))
(check-equal? (list-update (list 5 6 7 8 9) 2 add1)
              (list 5 6 8 8 9))
(check-equal? (list-update (list 5 6 7 8 9) 4 add1)
              (list 5 6 7 8 10))

(let-values ([(v l) (list-ref/update (list 5 6 7 8 9) 2 add1)])
  (check-equal? v 7)
  (check-equal? l (list 5 6 8 8 9)))

(check-equal? (list) empty)
(check-equal? (list 1 2 3) (cons 1 (cons 2 (cons 3 empty))))

(check-equal? (list* empty) empty)
(check-equal? (list* 1 2 3 empty) (list 1 2 3))
(check-equal? (list* 1 2 3 (list 4 5)) (list 1 2 3 4 5))

; Intentionally removed now that improper lists are supported.
;(check-exn exn? (λ () (list* 5)))
(check-equal? (list* 5) 5)
(check-equal? (list* 1 2) (cons 1 2))

(check-equal? (build-list 0 (lambda (i) i)) empty)
(check-equal? (build-list 5 (lambda (i) i))
              (list 0 1 2 3 4))
(check-equal? (build-list 5 add1)
              (list 1 2 3 4 5))

(check-equal? (make-list 0 'a) empty)
(check-equal? (make-list 5 'a)
              (list 'a 'a 'a 'a 'a))

(check-equal? (length empty) 0)
(check-equal? (length (cons 9 empty)) 1)
(check-equal? (length (build-list 10 (lambda (i) i))) 10)

(check-equal? (count #false) 0)
(check-equal? (count empty) 0)
(check-equal? (count (cons 9 empty)) 1)
(check-equal? (count (build-list 10 (lambda (i) i))) 10)
(check-equal? (count (list* 1 2 3)) 2)

(check-equal? (list-tail empty 0) empty)
(check-equal? (list-tail (cons 9 empty) 1) empty)
(check-equal? (list-tail (list 0 1 2 3 4) 2)
              (list 2 3 4))

(check-equal? (map add1 (list 0 1 2 3))
              (list 1 2 3 4))
(check-equal? (map (lambda (x y) (+ x y)) (list 1 2 3) (list 4 5 6))
              (list 5 7 9))
(check-exn exn? (λ () (map add1 (list 1 2 3) (list 4 5 6))))
(check-exn exn? (λ () (map + (list 1 2 3) (list 4))))

(check-equal? (foldr list* empty (list 1 2 3) (list 4 5 6))
              (list 1 4 2 5 3 6))
(check-exn exn? (λ () (foldr add1 (list 1 2 3) (list 4 5 6))))
(check-exn exn? (λ () (foldr + (list 1 2 3) (list 4))))

(check-equal? (foldl list* empty (list 1 2 3) (list 4 5 6))
              (list 3 6 2 5 1 4))
(check-exn exn? (λ () (foldl add1 (list 1 2 3) (list 4 5 6))))
(check-exn exn? (λ () (foldl + (list 1 2 3) (list 4))))

(check-equal? (andmap positive? (list 1 2 3)) true)
(check-equal? (andmap positive? (list 1 -2 'a)) false)
(check-equal? (andmap + (list 1 2 3) (list 4 5 6)) 9)
(check-exn exn? (λ () (andmap positive? (list 1 2 'a))))

(check-equal? (ormap eq? (list 'a 'b 'c) (list 'a 'b 'c)) true)
(check-equal? (ormap positive? (list 1 2 'a)) true)
(check-equal? (ormap + (list 1 2 3) (list 4 5 6)) 5)

(check-equal? (append empty empty) empty)
(check-equal? (append empty (list 1 2 3)) (list 1 2 3))
(check-equal? (append (list 1 2 3) empty) (list 1 2 3))
(check-equal? (append (list 1 2 3) (list 4 5 6))
              (list 1 2 3 4 5 6))

(check-equal? (append) empty)
(check-equal? (append (list 1 2)) (list 1 2))
(check-equal? (append (list 1 2) (list 3 4) (list 5 6))
              (list 1 2 3 4 5 6))

(check-equal? (reverse empty) empty)
(check-equal? (reverse (cons 9 empty)) (cons 9 empty))
(check-equal? (reverse (list 0 1 2 3 4))
              (list 4 3 2 1 0))

(check-equal? (range 0)  (list))
(check-equal? (range 1)  (list 0))
(check-equal? (range 10) (list 0 1 2 3 4 5 6 7 8 9))

(check-equal? (for/last ([i (in-list (list))]) i) false)
(check-equal? (for/last ([i (in-list (list 1 2))]) i) 2)
(check-equal? (for/last ([i (in-list (list 1 2 3 4))]) i) 4)

(check-equal? (for/list ([i '(1 2 3)]
                         [j "abc"]
                         #:when (odd? i)
                         [k #(#t #f)])
                (list i j k))
              (list (list 1 #\a #t) 
                    (list 1 #\a #f) 
                    (list 3 #\c #t) 
                    (list 3 #\c #f)))
(check-equal? (for/list () 'any)
              (list 'any))
(check-equal? (cons 1 empty)
              (cons 1 empty))
(check-equal? (for/list ([i '()])
                (error "doesn't get here"))
              empty)

(check-exn exn? (λ () (list-ref empty 0)))
(check-exn exn? (λ () (list-ref (cons 9 empty) 1)))
(check-exn exn? (λ () (list-set empty 0)))
(check-exn exn? (λ () (list-set (cons 9 empty) 1)))