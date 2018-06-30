#lang racket

#|

There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, 5C3 = 10.

In general,
nCr = 	
n!
r!(n−r)!
	,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.

It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?

|#

(require "utils.rkt")

(define p-triangle
  (memoize
   (λ (row column)
     (cond
       ((or (zero? column) (zero? row)) 1)
       (else (+
              (p-triangle (sub1 row) column)
              (p-triangle row (sub1 column))))))))

(define (pascals-triangle n-rows acc)
  (define (nth-row n x)
    ;; we are going up diagonally from left side
    (cond
      [(< n 0) '()]
      [else (cons (p-triangle n x) (nth-row (sub1 n) (add1 x)))]))
  (cond
    [(< n-rows 0) acc]
    [else (pascals-triangle (sub1 n-rows) (cons (nth-row n-rows 0) acc))]))

(length (filter (λ (x) (> x 1000000)) (apply append (pascals-triangle 100 '()))))
