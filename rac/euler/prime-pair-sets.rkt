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

;; a set of primes that can be concatenated in all ways to make other primes

(define (two-combs li)
  (filter
   identity
   (append-map (λ (x)
                 (map (λ (y)
                        (if (= x y) #f
                            (list x y))) li)) li)))

;; candidate for utils.rkt
(define (integer-cat n1 n2)
  (list->integer (append (integer->list n1) (integer->list n2))))

(define (check li)

  (define (helper combs)
    (cond
      [(null? combs) #t]
      [(not (prime? (apply integer-cat (car combs)))) #f]
      [else (helper (cdr combs))]))

  (and (andmap prime? li) (helper (two-combs li))))
