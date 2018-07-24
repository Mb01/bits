#lang racket

;; The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

;; Find the smallest cube for which exactly five permutations of its digits are cube.


;; ::: it's probably best to just sort all the cubes and count how many are the same
;; ::: then we find the smallest cube that is permutation (sorts to the same string) as one that has 5 permutations


(require "utils.rkt")

(define cubes (map (λ (x) (* x x x)) (range 1 9000)))

;; this has to remain a list, otherwise 0 dissapears
(define sorted-cubes (map sort-number cubes))

(define (list-count li)
  (define (counter li ht)
    (cond
      [(null? li) ht]
      [(hash-has-key? ht (car li)) ; if we already added at that permutation
       (counter (cdr li) ; recurse with rest of list
                (hash-set ht (car li) (add1 (hash-ref ht (car li)))))] ; add 1 to count in table
      [else (counter (cdr li) (hash-set ht (car li) 1))])) ; else initialize the value
  
  (counter li (hash))) ; call the helper function

;; this gives us a sorted permutation, let's use that to find the cube
(define perm (caar (filter (λ (x) (= (cdr x) 5)) (hash->list (list-count sorted-cubes)))))

(define (find cubes) ; we know we will find it
  (if (equal? (sort-number (car cubes)) perm) (car cubes)
      (find (cdr cubes))))

(find cubes)
