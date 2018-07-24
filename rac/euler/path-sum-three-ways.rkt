#lang racket

;; data file: path-sum-two-ways-data.txt
;; hope its the same fille

;; The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column and finishing in any cell in the right column, and only moving up, down, and right, is indicated in red and bold; the sum is equal to 994.


;; Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."), a 31K text file containing a 80 by 80 matrix, from the left column to the right column.

;; :: at least solvable with dijkstra

(define infinity +inf.0)

(define dim 5)

(define _in-file (open-input-file "path-sum-two-ways-data.txt"))
(define _data (port->string _in-file))

(define cost-table (list->vector (map (λ (lin) (list->vector (map string->number (string-split lin ",")))) (string-split _data "\n"))))

(define distance-table (vector-map (lambda (x) (make-vector dim infinity)) (make-vector dim)))

(define (table-ref table x y)
  (if (and (>= x 0) (>= y 0))
      (vector-ref (vector-ref table y) x)
      infinity))

(define (table-set! x y n)
  (vector-set! (vector-ref distance-table y) x n))


;; I think we can still build a table, 

;; no, at this point we might need a graph

;; or is there an order we can move in
;; we can always rely on not being able to move left
;; which guarantees lefter elements don't need updates


; (define (build-table x y)






