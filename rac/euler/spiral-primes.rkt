#lang racket

;; Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

;; 37 36 35 34 33 32 31
;; 38 17 16 15 14 13 30
;; 39 18  5  4  3 12 29
;; 40 19  6  1  2 11 28
;; 41 20  7  8  9 10 27
;; 42 21 22 23 24 25 26
;; 43 44 45 46 47 48 49

;; It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

;; If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?
;;..................................................

;; so the diagonals are [odd-squares that-minus-(n-1) that-minus-(n-1) that-minus-(n-1)]
;; that is (n^2, n^2 - (n - 1), n^2 - 2(n - 1), 3^2 - 3(n - 1)
(require "utils.rkt")

(define (diags n) ; give me odd numbers only!
  (let* ([n-1 (- n 1)]
         [a (* n n)]
         [b (- a n-1)]
         [c (- b n-1)]
         [d (- c n-1)])
    (list a b c d)))

(define highest '())

(define (count-primes li)
  (let ([primes (filter-map prime? li)])
    (unless (null? primes) (set! highest li))
    (length primes)))

(define (search odd prime-count total-count) ;; remember to count the one in the middle of the square 
    (let* ([corners (diags odd)]
           [prime-count (+ prime-count (count-primes corners))]
           [total-count (+ total-count 4)])
      (if (< (/ prime-count total-count) 0.1)
          odd
          (search (+ 2 odd) prime-count total-count))))

(define first-odd 3)
(define empty-count 0)
(define num-center-squares 1)

(search first-odd empty-count num-center-squares)

