#lang racket
(require rackunit
         "../benchmarks/garden-fence.rkt")

(provide garden-fence-tests)

(define (make-garden-suite name encrypt decrypt)
  (test-suite 
   name
   (check-equal? (encrypt "" 5) "")
   (check-equal? (encrypt "diesisteinklartext" 2)
                 "deitikatxissenlret")
   (check-equal? (encrypt "diesisteinklartext" 6)
                 "dkinleiasertittxse")
   (check-equal? (encrypt "diesisteinklartext" 18)
                 "diesisteinklartext")
   (check-equal? (encrypt "PROGRAMMING PRAXIS" 4)
                 "PMPRAM RSORIGAIGNX")
   
   (check-equal? (decrypt "" 5) "")
   (check-equal? (decrypt "deitikatxissenlret" 2)
                 "diesisteinklartext")
   (check-equal? (decrypt "dkinleiasertittxse" 6)
                 "diesisteinklartext")
   (check-equal? (decrypt "diesisteinklartext" 18)
                 "diesisteinklartext")
   (check-equal? (decrypt "PMPRAM RSORIGAIGNX" 4)
                 "PROGRAMMING PRAXIS")))

(define garden-fence-tests
  (make-test-suite "garden-fence-tests"
                   (map (lambda (c)
                          (make-garden-suite (crypt-name c)
                                             (crypt-en c)
                                             (crypt-de c)))
                          crypts)))
