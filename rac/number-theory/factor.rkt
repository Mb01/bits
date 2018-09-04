#lang racket

;; naive approach
(define (factor n)
  (define (inner x)
    (if (zero? (modulo n x))
        (cons x (factor (/ n x)))
        (inner (+ x 2))))
  (cond
    [(= n 1) '()]
    [(zero? (modulo n 2)) (cons 2 (factor (/ n 2)))]
    [else (inner 3)]))

;; less naive approach
(define (factor2 n)
  (define (inner n x)
    (define sqrt-n (sqrt n))
    (cond
      [(= n 1) '()] ; 1 is not a prime factor
      [(> x sqrt-n) `(,n)] ; n remains yet cannot be divided
      [(zero? (modulo n x)) (cons x (inner (/ n x) x))] ;; found a factor
      [else (inner n (+ x 2))])) ; keep searching
  (if (zero? (modulo n 2)) (cons 2 (factor2 (/ n 2)))
      (inner n 3)))


