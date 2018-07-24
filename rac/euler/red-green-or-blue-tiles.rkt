#lang racket

;; A row of five black square tiles is to have a number of its tiles replaced with coloured oblong tiles chosen from red (length two), green (length three), or blue (length four).
;; If red tiles are chosen there are exactly seven ways this can be done.
;; If green tiles are chosen there are three ways.
;; And if blue tiles are chosen there are two ways.
;; Assuming that __colours cannot be mixed__ there are 7 + 3 + 2 = 12 ways of replacing the black tiles in a row measuring five units in length.
;; How many different ways can the black tiles in a row measuring
;; -((fifty units))- in length be replaced if colours cannot be mixed and at least one coloured tile must be used?

;; :: duuude this is the partitioning problem again, this time with permutations
;; :: we already solved this problem by accident

;; ok so let's just solve this over again

;; 1, 2, 3, 4 (one can be mixed but also can't be exclusively used)

(require "utils.rkt")

(define partitions
  (memoize
   (lambda (n coin)
     (cond
       [(< n 0) 0] ;; tried too much
       [(= n 0) 1] ;; found a way
       [else (+ (partitions (- n 1) coin) (partitions (- n coin) coin))]))))

(define (partitions-1 n coin)
  (sub1 (partitions n coin)))

(+ (partitions-1 50 2) (partitions-1 50 3) (partitions-1 50 4))
