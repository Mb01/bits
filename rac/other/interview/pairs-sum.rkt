#lang racket

;;How do you find all pairs of an integer array whose sum is equal to a given number?

;; pairs need to be unique, not swapped around

;; so we need to find two integers in the array that sum to a number
;; when we consider the number
;; all we need to know is whether the other number is in the list
;; binary search would be best
;; but we probably can do with a with a hash table

(define (search-pairs li add-to)
  (define li-set (list->set li))
  (let ([found
         (filter
          (lambda (x) x)
          (map
           (lambda (x)
             (if (set-member? li-set (- add-to x))
                 (list x (- add-to x))
                 #f))
           li))])
    ; get rid of non-unique solutions
    (filter (lambda (x) (< (car x) (cadr x))) found)))

; my algorithm still throws away (4 4) for 8
(display (search-pairs '(3 4 5 2 1 6 0 5 3 8 21 5 2) 8))

                       
