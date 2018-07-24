#lang racket

;; Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.

;; By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7. (** 3/7ths, oh come on, :- **)

;; :: first off, we don't need to generate any fractions greater than 2/5

;; let's see if that is reasonable
(define one-million 1000000)

(define (fractions-lt-2/5 n d) ;; -> list of fractions with denominator d
  (cond
    [(>= (/ n d) 2/5) (/ (sub1 n) d)]
    [else (fractions-lt-2/5 (add1 n) d)]))

;; now we need to find the largest fraction we can generate under 2/5 and reduce it
;; awesomely enough, racket has native fractions and reduces them
;; ... it turns out this would take ages

(define (search-limit limit)
  (define (search d rightmost)
    (when (zero? (modulo d 1000)) (displayln d))
    (let* ([frac (fractions-lt-2/5 1 d)]
           [champion (max frac rightmost)])
      (cond
        [(> d limit) rightmost] ; don't return new champion
        [else (search (add1 d) champion)])))
  (search 1 0))

; (map search-limit (range 1 20))
;'(0 0 1/3 1/3 1/3 1/3 1/3 3/8 3/8 3/8 3/8 3/8 5/13 5/13 5/13 5/13 5/13 7/18 7/18)

;; ok, I have a better solution
;; a. increase the numerator until 2/5 (track maximum)
;; b. increase the denominator
;; c. if den > one-million return else repeat

(define (increase-numerator n d)
  (cond
    [(>= (/ n d) 3/7) (sub1 n)]
    [else (increase-numerator (add1 n) d)]))

(define (search n d rightmost)
  (let* ([best (max rightmost (/ (increase-numerator n d) d))]
        [n (numerator best)]) ;; after increasing d, the numerator must also increas
      ;(when (zero? (modulo d 1000)) (displayln (list d best)))
    (cond
      [(> d one-million) best] ;; keep new best for one-millionth exactly
      [else (search n (add1 d) best)]))) ;; track new best

(search 0 1 0)
