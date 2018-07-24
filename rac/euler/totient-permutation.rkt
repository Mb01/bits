#lang racket



;; Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
;; The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

;; Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

;; Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

(require "utils.rkt")


(define ten-million 10000000)

(define (totient n)

  (define uniq-factors (remove-duplicates (factor n)))
  
  (define (flopping-product fact-combs)
    (if (null? fact-combs) 1
      (* (car fact-combs) (- (flopping-product (cdr fact-combs))))))
              
  (let* ([products (map flopping-product (combinations uniq-factors))]
         [n/prods (map (lambda (x) (/ n x)) products)])
    (apply + n/prods)))

(define (permutations? n1 n2)
  (equal? (sort-number n1) (sort-number n2)))

(define (totient-permutation? n)
  (permutations? n (totient n)))

(define (totient-permutation-ratio n)
  (if (totient-permutation? n)
      (/ n (totient n))
      #f))

(apply min (filter-map totient-permutation-ratio (range 2 ten-million)))
