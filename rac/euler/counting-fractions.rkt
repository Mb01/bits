#lang racket

;; Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

;; It can be seen that there are 21 elements in this set.

;; How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?

;; ::: SPOILER ALERT :::

(require "utils.rkt")

(define one-million 1000001)

;; :: a fraction can be reduced if both the numerator and denominator has
;; :: a common factor
;; :: a reduced fraction has a smaller denominator
;; :: at some point between one and one-million
;; :: we have already counted it

;; :: a more difficult question might be
;; :: how many reduced proper fractions are there between a and b

;; first attempt
;; literally count how many numerators 
;; from 1 to n-1 cannot be reduced
;; by seeing if they are divisible by a factor of a given denominator


(define (divisible? x y)
  (= 0 (modulo x y)))

(define (divisible-by-any? x li)
  (ormap (λ (y) (divisible? x y)) li))

(define (irreducible-numerators denominator)
  (let ([factors (remove-duplicates (factor denominator))])
        (filter-not (curryr divisible-by-any? factors) (range 1 denominator))))

(define (count-numerators d)
  (length (irreducible-numerators d)))

;;(apply + (append-map count-numerators (range 2 one-million)))
;; yikes, nope, not one minute

;; second attempt
;; I'm proud to have found an elegant solution.
;; this is deep, this is beautiful

;; The number of ways a numerator can be reduced
;; for any factor is n / factor

;; sum
;; n over each odd-length combinations of its factors multiplied together 
;; minus n over each even-length combinations of its factors multiplied together 

;; or

;; the number of times a factor divides n is counted as positive when
;; having an odd number of prime factors and negative when having an odd
;; number of prime factors


;; we start with
;; n divided by the (product of no factors) 1 is n
;; it can be reduced by (n / f1) + (n / f2) ... (n / fn) ways
;; but we overcounted by (n / f1*f2) + (n / f1*f3) ... (n / fn*fn+1) ways
;; but we overcounted the overcount by (n / f1*f2*f3) ... ways
;; but we overcounted the overcount of the overcount by .... ways

(define (sum-irreducible n uniq-factors)

  (define (flopping-product fact-combs)
    (if (null? fact-combs) 1
      (* (car fact-combs) (- (flopping-product (cdr fact-combs))))))
              
  (let* ([products (map flopping-product (combinations uniq-factors))]
         [n/prods (map (lambda (x) (/ n x)) products)])
    (apply + n/prods)))

;; (apply + (map (lambda (x) (sum-irreducible x (remove-duplicates (factor x)))) (range 2 9))); -> 21

;(apply + (map (λ (x) (sum-irreducible x (remove-duplicates (factor x)))) (range 2 one-million)))

(sum-irreducible 87109 (factor 87109))
