#lang racket
#|
Take the number 192 and multiply it by each of 1, 2, and 3:

    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
|#


;; constraints n > 1, 9-digit pandigital, concatenated product of consecutive integers,  
;; largest pandigital number is 987654321
;; I asssume that the concatenation begins with the 1 product
;; 

#|
What is the key insight here?

The 1 product will have the number as is
The 2 product ends in 0,2,4,6,8
The 3 product adds to 3,6,or 9
the 4 product ends in 0,2,4,6,8

|#

(require "utils.rkt")

(define (pandigital? n)
  (equal? (qsort (integer->list n)) '(1 2 3 4 5 6 7 8 9)))

(define (pand li)
  (pandigital? (number-cat li)))

(define (pd? n)
  (define (make-range x)
    (map (λ (y) (* n y)) (range 1 x)))
  (define (check-range x)
    (pand (make-range x)))
  (cond
    ((check-range 3) (list 3 n))
    ((check-range 4) (list 4 n))
    ((check-range 5) (list 5 n))
    ((check-range 6) (list 6 n))
    ((check-range 7) (list 7 n))
    ((check-range 8) (list 8 n))
    ((check-range 9) (list 9 n))
    (else #f)))

;; finishing the answer redacted



