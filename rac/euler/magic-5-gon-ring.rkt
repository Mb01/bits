#lang racket

;; Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.

;; Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.

;              4
;               \
;                3
;               / \
;              1 - 2 - 6
;             /
;            5

;; It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
;; Total	Solution Set
;; 9	4,2,3; 5,3,1; 6,1,2
;; 9	4,3,2; 6,2,1; 5,1,3
;; 10	2,3,5; 4,5,1; 6,1,3
;; 10	2,5,3; 6,3,1; 4,1,5
;; 11	1,4,6; 3,6,2; 5,2,4
;; 11	1,6,4; 5,4,2; 3,2,6
;; 12	1,5,6; 2,6,4; 3,4,5
;; 12	1,6,5; 3,5,4; 2,4,6

;; By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.

;; Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum 16-digit string for a "magic" 5-gon ring?

;               a
;                \
;                 b     d
;               /   \  /
;             i       c
;            / \     /
;           j   g - e - f
;                \ 
;                 h

;; :: sums abc = dce = fge = hgi = jib
;; :: always starting with "a" as the lowest external node
;; :: so recursive search down the tree should exclude higher external nodes

;; there are many ways to limit the search space of this problem
;; 1. restrict searches to valid "a d f h j" where a is the smallest
;; 2. ***abc determines the rest of the entries***
;; 3. there are limits on what total can be, larger number externals reduce the total but increase the maximum 16-digit string

;; -> thus the answer has the lowest sum possible -> 14

(require "utils.rkt")

;; I've just defined all the constraints in a long-handed manner.

(define (solution? a b c d e f g h i j)
  (and
   ; a is smallest
   (< a d f h j)
   ; the sums are the same
   (= (+ a b c)
      (+ d c e)
      (+ f e g)
      (+ h g i)
      (+ j i b))))

(define (abc a b c)
  (+ a b c))

(define (dce a b c d e)
  (if (< a d) (+ d c e) 0))

(define (feg a b c d e f g)
  (if (< a f) (+ f e g) 0))

(define (hgi a b c d e f g h i)
  (if (< a h) (+ h g i) 0))

(define (jib a b c d e f g h i j)
  (if (< a j) (+ j i b) 0))

(define (catenated a b c d e f g h i j)
  (number-cat `(,a ,b ,c ,d ,c ,e ,f ,e ,g ,h ,g ,i ,j ,i ,b)))


;; args are
;; rem: remaining to be chosent
;; acc: accumulated answer for current depth

;; lowest sum is 14, this should apply to the solution
(define sum 14)

(define (search rem acc)
  (let ([len (length acc)]
        [li-#f (list #f)])
  (cond
    ;; complete to three, test sub-solution
    [(and (= len  3) (not (= (apply abc acc) sum))) li-#f]
    ;; complete to five, test sub-solution
    [(and (= len 5)  (not (= (apply dce acc) sum))) li-#f]
    ;; complete to seven, test sub-solution
    [(and (= len 7)  (not (= (apply feg acc) sum))) li-#f]
    ;; complete to nine, test sub-solution
    [(and (= len 9)  (not (= (apply hgi acc) sum))) li-#f]
    ;; complete to ten, test solution
    [(and (= len 10) (= (apply jib acc) sum)) (list acc)]
    ;; recurse
    [else (append-map (λ (try) (search (remove try rem) (append acc (list try)))) rem)])))

;; all the magic 5-gons for defined sum
(define solutions (filter identity (search '(1 2 3 4 5 6 7 8 9 10) '())))

;; maxiumum solution
(car (sort (map (λ (x) (apply catenated x)) solutions) >))

;; and it turns out to be the correct solution.
