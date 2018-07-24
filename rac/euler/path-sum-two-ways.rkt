#lang racket

;;data file: path-sum-two-ways-data.txt

;; In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only moving to the right and down, is indicated in bold red and is equal to 2427.

;; Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."), a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.

;; :: at least solvable with dijkstra

(define infinity +inf.0)

(define dim 80)

(define _in-file (open-input-file "path-sum-two-ways-data.txt"))
(define _data (port->string _in-file))

(define cost-table (list->vector (map (λ (lin) (list->vector (map string->number (string-split lin ",")))) (string-split _data "\n"))))

(define distance-table (vector-map (lambda (x) (make-vector dim)) (make-vector dim)))

(define (table-ref table x y)
  (if (and (>= x 0) (>= y 0))
      (vector-ref (vector-ref table y) x)
      infinity))

(define (table-set! x y n)
  (vector-set! (vector-ref distance-table y) x n))

(define (build-table x y)

  (define (left)
    (table-ref distance-table (- x 1) y))

  (define (up)
    (table-ref distance-table x (- y 1)))

  (cond
    [(= x dim) (void)]
    [(= y dim) (build-table (add1 x) 0)]
    [else (table-set! x y (+ (table-ref cost-table x y) (min (up) (left))))
          (build-table x (add1 y))]))

(table-set! 0 0 (table-ref cost-table 0 0))
(build-table 0 1) ; we start at (0, 0)

(table-ref distance-table 79 79)




