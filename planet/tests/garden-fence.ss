#lang scheme
(require planet/version
         rackunit
         (this-package-in benchmarks/garden-fence))

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

(define (hack n)
  (let ((c (list-ref crypts n)))
    (make-garden-suite (crypt-name c)
                       (crypt-en c)
                       (crypt-de c))))

;; A hack because there is no make-test-suite function.  See also:
;; http://list.cs.brown.edu/pipermail/plt-scheme/2009-May/032742.html
(define/provide-test-suite garden-fence-tests
  (hack 0)
  (hack 1)
  (hack 2)
  (hack 3)
  (hack 4)
  (hack 5)
  (hack 6))
