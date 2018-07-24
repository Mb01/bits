#lang racket

;; It is possible to write ten as the sum of primes in exactly five different ways

;; 7 + 3
;; 5 + 5
;; 5 + 3 + 2
;; 3 + 3 + 2 + 2
;; 2 + 2 + 2 + 2 + 2

;; What is the first value which can be written as the sum of primes in over five thousand different ways?

;; we're back to integer partitioning
(require "utils.rkt")

(define primes (reverse (primes-to 100)))

(define partition
  (memoize
   (λ (n li) ;; li contains candidates
     (cond
       [(< n 0) 0]
       [(zero? n) 1]
       [(null? li) 0]
       ;; we can use and not use a prime
       [else (+ (partition (- n (car li)) li)
                (partition n (cdr li)))]))))
             
(define (search n)
  (cond
    [(> (partition n primes) 5000) n]
    [else (search (add1 n))]))

(search 1)
