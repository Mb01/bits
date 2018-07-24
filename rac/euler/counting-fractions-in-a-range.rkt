#lang racket



;; Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

;; It can be seen that there are 3 fractions between 1/3 and 1/2.

;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?


;; :: original approach
;; :: 1. find all of the fractions that are just larger than 1/3 by
;; ::    starting with 1/3 -> 1/4 -> 1/5 -> 2/5
;; ::    we know that increasing the denominator makes the number smaller
;; ::    so we never have to decrease the numerator
;; :: 2. store the largest numerators
;; :: 3. find all that are just smaller than 1/2
;; ::    starting with 1/2 -> 1/3 -> 2/3 -> 1/4 -> 2/5
;; :: 4. store the largest numerators
;; :: 5. substract respective numerators
;; ::    1/2, 1/2 = 0 ... 2/7, 3/7 = 1

(define stop-at 12001)
;(define stop-at 20)

;; all numerators for less-than-equal-to limit
(define (max-numerators n d limit acc); valid n, d = (/ n d) < limit
  ;;(println (list n d))
  (cond
    [(= d stop-at) (reverse acc)]
    [(>= (/ n d) limit) (max-numerators n (add1 d) limit (cons (- n 1) acc))]
    [else (max-numerators (add1 n) d limit acc)]))

(define (min-numerators n d limit acc)
  (cond
    [(= d stop-at) (reverse acc)]
    [(> (/ n d) limit) (min-numerators n (add1 d) limit (cons (- n 1) acc))]
    [else (min-numerators (add1 n) d limit acc)]))

; (apply + (map - (max-numerators 1 1 1/2 '()) (min-numerators 1 1 1/3 '())))
;; 11,996,000

;; ... right, reduced fractions

;; ok, so we were given a very reasonable number like 12000 to work with

;; so we can just generate all the fractions and reduce them
;; there's only 12 million of them

;; I bet we can even use the information from the functions above

(define maxes (max-numerators 1 1 1/2 '()))
(define mins (map add1 (min-numerators 1 1 1/3 '())))

(define (all-fractions-between n1 n2 d)
  (cond
    [(> n1 n2) '()]
    [else (cons (/ n1 d) (all-fractions-between (add1 n1) n2 d))]))

(define (reduce mins maxes d acc)
  (cond
    [(= d stop-at) (length (set->list acc))]
    [else (reduce (cdr mins) (cdr maxes) (add1 d)
                  (set-union
                   acc
                   (apply seteqv
                          (all-fractions-between (car mins) (car maxes) d))))]))

(reduce mins maxes 1 (seteqv))



