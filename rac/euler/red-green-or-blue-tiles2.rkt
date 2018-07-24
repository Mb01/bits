#lang racket

;; Using a combination of black square tiles and oblong tiles chosen from: red tiles measuring two units, green tiles measuring three units, and blue tiles ;; measuring four units, it is possible to tile a row measuring five units in length in exactly fifteen different ways.

;; :: duuude this is the partitioning problem again, this time with permutations 
;; :: we already solved this problem by accident

;; ok so let's just solve this over again

;; 1, 2, 3, 4 can be mixed this time

(require "utils.rkt")

(define partitions
  (memoize
   (lambda (n)
     (cond
       [(< n 0) 0] ;; tried too much
       [(= n 0) 1] ;; found a way
       [else (apply + (map partitions (range (- n 4) n)))]))))

(partitions 50)


