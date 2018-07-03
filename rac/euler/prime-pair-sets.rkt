#lang racket

#|
The primes 3, 7, 109, and 673, are quite remarkable. 
By taking any two primes and concatenating them in any order 
the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. 
The sum of these four primes, 792, represents the lowest 
sum for a set of four primes with this property.
Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
|#

(require "utils.rkt")

;; we're looking for  a set of primes that can be concatenated in all ways to make other primes
;; I think I will start by:
;; trying to add primes to a set until I get five 

;; prototype for checking answer
(define (concat-all-ways li)
  (map number-cat (permutations-n li 2)))
;;(concat-all-ways '(1 2 3)) -> '(12 13 21 23 31 32)

(define (check-solution li)
  (andmap prime? (concat-all-ways li)))

#|

Now to search for a solution

|#

(define prime-cat? ; can number e concatenated both ways and still be prime?
  (memoize ; these will be unique so we don't need to memoize, the order of the combinations is also always ascending
   (lambda (s1 s2)
     (andmap prime? (list (number-cat (list s1 s2)) (number-cat (list s2 s1)))))))

(define (can-add? n li) ; can we add the number to our set
  (andmap (λ (x) (prime-cat? n x)) li))

(define (search primes acc)
  (cond
    ;; see if current track is wrong
    [(and (not (null? acc)) (not (can-add? (car acc) (cdr acc)))) '(#f)] 
    [(= 5 (length acc)) acc]
    [(null? primes) '(#f)]
    [else 
     (append
      (search (cdr primes) (cons (car primes) acc))
      (search (cdr primes) acc))]))

;; redacted
