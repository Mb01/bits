;; The first two consecutive numbers to have two distinct prime factors are:

;; 14 = 2 × 7
;; 15 = 3 × 5

;; The first three consecutive numbers to have three distinct prime factors are:

;; 644 = 2² × 7 × 23
;; 645 = 3 × 5 × 43
;; 646 = 2 × 17 × 19.

;; Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?

#lang racket

(require "utils.rkt")
;; definately a candidate for memoize
;; factor has four unique factors
(define (factor-len4 n)
  (let ([factor-n (factor n)]) 
     (= 4 (length (remove-duplicates factor-n)))))

(define (solve i)
  (if
   (and
    (factor-len4 i)
    (factor-len4 (+ i 1))
    (factor-len4 (+ i 2))
    (factor-len4 (+ i 3)))
   i
   (solve (add1 i))));and pray there is an answer

(solve 15)
