#lang racket

;; A row measuring seven units in length has red blocks with a minimum length of three units placed on it, such that any two red blocks (which are allowed to be different lengths) are separated by at least one black square. There are exactly seventeen ways of doing this.

;;How many ways can a row measuring fifty units in length be filled?

;; NOTE: Although the example above does not lend itself to the possibility, in general it is permitted to mix block sizes. For example, on a row measuring eight units in length you could use red (3), black (1), and red (4).

(require "utils.rkt")


;; :: all we need to do is call an intermediate function that chooses black, but also checks the base case
;; :: or just track last choice

(define (choose-black n)
  (partitions (sub1 n) #f))

(define (choose-red n)
  (apply + (map (λ (x) (partitions x #t)) (range 0 (- n 2)))))

(define partitions
  (memoize
   (lambda (n last-red)
     (cond
       [(< n 0) 0] ;; tried too much
       [(= n 0) 1] ;; found a way
       [last-red (choose-black n)]
       [else (+ (choose-black n) (choose-red n))]))))

(partitions 50 #f)
