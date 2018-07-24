#lang racket

;; NOTE: This is a more difficult version of Problem 114.

;; A **row measuring n units** in length has red blocks with a **minimum length of m** units placed on it, such that any two red blocks (which are allowed to be different lengths) are separated by at least one black square.
;; Let the fill-count function, F(m, n), represent the number of ways that a row can be filled.
;; For example, F(3, 29) = 673135 and F(3, 30) = 1089155.
;; That is, for m = 3, it can be seen that n = 30 is the smallest value for which the fill-count function first exceeds one million.
;; In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count function first exceeds one million.
;; For m = 50, find the least value of n for which the fill-count function first exceeds one million.
;; :: find the first length that exceeds one million for m = 50

(require "utils.rkt")

;; :: all we need to do is call an intermediate function that chooses black, but also checks the base case
;; :: or just track last choice
;; :: set m to 50 and keep trying bigger numbers

(define m 50)
(define one-million 1000000)

(define (choose-black n)
  (partitions (sub1 n) #f))

(define (choose-red n)
  (apply + (map (λ (x) (partitions x #t)) (range 0 (- n (- m 1))))))

(define partitions
  (memoize
   (lambda (n last-red)
     (cond
       [(< n 0) 0] ;; tried too much
       [(= n 0) 1] ;; found a way
       [last-red (choose-black n)]
       [else (+ (choose-black n) (choose-red n))]))))

(define (find-answer n)
  (if (> (partitions n #f) one-million) n
      (find-answer (add1 n))))

(find-answer 0)
