#lang racket

(require rackunit
         "utils.rkt")

;; memoize
;; we'll use fibonacci sequence to ensure runtime is appropriate
(define fibo
  (memoize
   (lambda (n)
     (cond
       [(< n 1) 0]
       [(= n 1) 1]
       [else (+ (fibo (- n 1)) (fibo (- n 2)))]))))

(check-true (let-values ([(result cpu-time real-time garbage-time) (time-apply fibo '(10000))])
                         (< cpu-time 1000)) "didn't expect memoized fibonacci to take that long")

;; prime
(check-true (andmap prime? '(5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199)))
(check-false (ormap prime? `(1 4 6 8 122 108 ,(* 17 19) ,(* 73 173))))

;; integer->list
(check-true (equal? (integer->list 1234) '(1 2 3 4)))

;; list->integer
(check-equal? (list->integer '(1 2 3 4)) 1234)
(check-equal? (list->integer '(0 1 2 3 4)) 01234)
(check-equal? (list->integer '(0)) 0)

;; number-cat
(check-equal? (number-cat '(12 34)) 1234)

;; qsort
(check-equal? (qsort '(5 4 3 2 1)) '(1 2 3 4 5))
(check-equal? (qsort '(1 2 3 4 5)) '(1 2 3 4 5))
(check-equal? (qsort '(4 5 3 2 1)) '(1 2 3 4 5))
(check-equal? (qsort '(1)) '(1))
(check-equal? (qsort '()) '())

;; next-permutation
(check-equal? (next-permutation 123) 132)
(check-equal? (next-permutation 321) #f)

;; prev-permutation
(check-equal? (prev-permutation 132) 123)

;; two-combs
(check-equal? (two-combs '(1 2)) '((1 2)))
(check-equal? (two-combs '(1 2 3)) '((1 2) (1 3) (2 3)))
(check-equal? (two-combs '(1)) '()) ;; this might actually be an error but is actual output

;; permutations
(check-equal? (permutations '()) '(()))
(check-equal? (permutations '(1)) '((1)))
(check-equal? (permutations '(1 2)) '((1 2) (2 1)))
(check-equal? (permutations '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
;; permutations-n
;; there is one zero length permutation of an empty set
(check-equal? (permutations-n '() 0) '(()) ) ;; this makes mathematical sense but may cause errors, see next line
;; there are no one length permutations of an empty set
(check-equal? (permutations-n '() 1) '() ) ;; this type discrepancy makes mathematical sense but may cause errors
(check-equal? (permutations-n '(1) 1) '((1)) )
(check-equal? (permutations-n '(1 2) 1) '((1) (2)) )
(check-equal? (permutations-n '(1 2) 2) '((1 2) (2 1)) )

;; combinations
(check-equal? (combinations '()) '(()) )
(check-equal? (combinations '(1)) '((1) ()) )
(check-equal? (combinations '(1 2)) '((1 2) (1) (2) ()) )
;; combinations-n
;; test 0 length combinations do give '(()) for any input
(check-equal? (combinations-n '() 0) '(()) )
(check-equal? (combinations-n '(1) 0) '(()) )
(check-equal? (combinations-n '(1 2) 0) '(()) )
(check-equal? (combinations-n '(1 2 3) 0) '(()) )
(check-equal? (combinations-n '(1 2 3 4) 0) '(()) ) ;; I'm satisfied
;; test 1 length combinations are correct
(check-equal? (combinations-n '() 1) '() ) ; there are no one length combinations for empty set
(check-equal? (combinations-n '(1) 1) '((1)) )
(check-equal? (combinations-n '(1 2) 1) '((1) (2)) )
(check-equal? (combinations-n '(1 2 3) 1) '((1) (2) (3)) )
(check-equal? (combinations-n '(1 2 3 4) 1) '((1) (2) (3) (4)) )
;; test 2 length combinations are correct
(check-equal? (combinations-n '() 2) '() ) ; there are no two length combinations for empty set
(check-equal? (combinations-n '(1) 2) '() ) ; there are no two length combinations for singleton l
(check-equal? (combinations-n '(1 2) 2) '((1 2)) )
(check-equal? (combinations-n '(1 2 3) 2) '((1 2) (1 3) (2 3)) )
(check-equal? (combinations-n '(1 2 3 4) 2) '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) )
;; test 3 length combinations are correct
(check-equal? (combinations-n '() 3) '() ) ; there are no three length combinations for empty set
(check-equal? (combinations-n '(1) 3) '() ) ; there are no three length combinations for empty set
(check-equal? (combinations-n '(1 2) 3) '() ) ; there are no three length combinations for empty set
(check-equal? (combinations-n '(1 2 3) 3) '((1 2 3)) )
(check-equal? (combinations-n '(1 2 3 4) 3) '((1 2 3) (1 2 4) (1 3 4) (2 3 4)) ); I'm satisfied for now

;; inversion?
(check-equal? (inversion? '()) #f)
(check-equal? (inversion? '(1)) #f)
(check-equal? (inversion? '(1 1)) #f)
(check-equal? (inversion? '(1 1 1)) #f)

(check-equal? (inversion? '(1 2)) #f)
(check-equal? (inversion? '(1 2 3)) #f)
(check-equal? (inversion? '(1 2 3 13)) #f)

(check-equal? (inversion? '(2 1)) #t)
(check-equal? (inversion? '(1 3 2)) #t)
(check-equal? (inversion? '(1 2 1)) #t)
(check-equal? (inversion? '(1 1 1 2 1)) #t)
(check-equal? (inversion? '(1 1 1 1 13 25 24)) #t)

;; consx
(check-equal? (consx 1 '() ) '() )
(check-equal? (consx 1 '(()) ) '((1)) )
(check-equal? (consx 1 '(() ())) '((1) (1)))
(check-equal? (consx 1 '(() () ())) '((1) (1) (1)))
(check-equal? (consx 1 '((2))) '((1 2)))
(check-equal? (consx 1 '((2) (3))) '((1 2) (1 3)))
