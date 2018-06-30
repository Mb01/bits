#lang racket

#|
A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

For example,

44 → 32 → 13 → 10 → 1 → 1
85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
|#

;; we probably want to cache our answers
(require "utils.rkt")

;; sum square of digits not square sum of digits
(define (sum-square-digits n)
  (let* ([nli (map (λ (x) (* x x)) (integer->list n))]
         [sum (apply + nli)])
    sum))

(define square-digit-chain
  (memoize
   (λ (n)
       (cond
         ((= 1 n)  #f)
         [(= 89 n) #t]
         [else (square-digit-chain (sum-square-digits n))]))))

;(length (map square-digit-chain (range 1 10000000)))
   
   

(define (search n acc)
  (cond
    [(= n 10000000) acc]
    [(square-digit-chain n) (search (add1 n) (add1 acc))]
    [else (search (add1 n) acc)]))

(search 1 0)
