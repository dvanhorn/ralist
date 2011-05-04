#lang racket
(require ;planet/version
         rackunit)
(require/expose "../main.ss" ;(this-package-in main)
                (#;build-tree node? make-node node-val node-left node-right
                            tree-val tree-ref tree-update
                            tree-map tree-map/n))

(define one 'a)
(define three  (make-node 'a 'b 'c))
(define seven 
  (make-node 'a 
             (make-node 'b 'c 'd)
             (make-node 'e 'f 'g)))

(define/provide-test-suite tree-tests
  
  (check-false (node? 'x))
  (check-true (node? (make-node 'x 'y 'z)))
  
  (check-equal? (node-val (make-node 'x 'y 'z)) 'x)
  
  (check-equal? (tree-ref 1 one 0) 'a)
  (check-equal? (tree-ref 3 three 1) 'b)
  (check-equal? (tree-ref 7 seven 6) 'g)
  
  (check-equal? (tree-update 1 one 0 (lambda (x) 'q)) 'q)
  (check-equal? (tree-update 3 three 1 (lambda (x) 'q))
                (make-node 'a 'q 'c))
  
  (check-equal? (tree-update 7 seven 6 (lambda (x) 'q))
                (make-node 'a 
                           (make-node 'b 'c 'd)
                           (make-node 'e 'f 'q)))

  (check-equal? (tree-map add1 0)
                1)
  (check-equal? (tree-map add1 (make-node 0 1 2))
                (make-node 1 2 3))
  
  
  (check-equal? (tree-map/n cons '(a z))
                '(a . z))
  
  (check-equal? (tree-map/n cons
                            (list (make-node 'a 'b 'c)
                                  (make-node 'z 'y 'x)))
                (make-node '(a . z) '(b . y) '(c . x)))
  
  #;(check-equal? (build-tree 1 (lambda (i) i)) 0)
  #;(check-equal? (build-tree 3 (lambda (i) i)) 
                (make-node 0 1 2))
  #;(check-equal? (build-tree 7 (lambda (i) i))
                (make-node 0
                           (make-node 1 2 3)
                           (make-node 4 5 6)))
  
  #;(check-equal? (build-tree 1 (lambda (i) 'x))
                'x)
  #;(check-equal? (build-tree 3 (lambda (i) 'x))
                (make-node 'x 'x 'x))
  
  
  )
