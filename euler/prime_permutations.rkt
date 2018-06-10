;;;;;;;;;;;;;;;;;;;;
;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

;; What 12-digit number do you form by concatenating the three terms in this sequence?
;;;;;;;;;;;;;;;;;;;;

;; 12 digit number implies 3 4 digit numbers

; obviously they need to be 4-digit numbers, they can't be odd numbers apart lest the second number be divisible by 2,

; this starts to look like (range 1000 9999 - 2)(+0 +1 +2)

#lang racket

(require "utils.rkt")

; so must define these here (as-written) because above functions rely on these
(define from 1000)
(define to 10000); not inclusive
(define max-x (/ (- to from) 2)); this is a big hint at where we can stop 


;;;;;;;;;;;;;;;;;;;;

#|
one approach is to generate all the increasing sequences (expensive) test whether they are permutations of each other (cheap) and test whether they are all prime (cheap) 
|#

; (inside the for loop)
(define (three-numbers-x-apart-from-n x n)
  (list n (+ n x) (+ n (* x 2))))

; specifies all possible n's (inner for loop) 
(define (arit-seqs-x-apart x)
  (map (lambda (n) (three-numbers-x-apart-from-n x n)) (range from (- to (* x 2)))))
; specifies all possible x's (outer for loop)
(define (arit-seqs)
  (map (lambda (x) (arit-seqs-x-apart x))  (range 1 max-x)))

; apply append (((1 2 3) (2 3 4))) -> ((1 2 3) (2 3 4))

#;(apply append
       (arit-seqs from to)); oops, not enough memory

; this turns out to be too expensive

;;;;;;;;;;;;;;;;;;;;

#|
another approach is finding all the prime numbers between 1000 and 10000
 and checking for a 3rd prime equally distant to combinations of 2 of the primes
2 3 -> 4 (not in list so no) 3 7 -> 11 (found a third prime)

then check whether they are permutations,  3 7 11 (no)
|#

(define primes (filter prime? (range from to)))

(define (are-permutations-of? n1 n2 n3)
  (let ([sort
      (lambda (x)
        (qsort (integer->list x)))])
    (and (equal? (sort n1) (sort n2))
         (equal? (sort n1) (sort n3)))))

(define (search2 primes)
  ; try to find numbers equally distant on the numberline
  (let* ([low (car primes)]
         [mid (car (cdr primes))]
         ; is there an (exactly) higher prime? and what is it
         [high-exists (memq (+ mid (- mid low)) primes)]
         [high (if high-exists (car high-exists) #f)])
    (cond
      ; stop if
      ((or
        (null? (cddr primes)) ; (nothing left to be third number)
        (> (+ mid ) (- mid low) 9999)) ; greatly reduce breadth to search
       #f)
      ; accept if
      ((and
        high-exists
        (are-permutations-of? low mid high)
        (list low mid high))) ; the answer

      ; discard second keep looking with other mids
      (else
       (search2 (cons (car primes) (cddr primes)))))))

(define (search primes)
  (let ([query (search2 primes)])
  (cond
    ((null? primes) #f)
    (query query) ; answer if answer
    (else (search (cdr primes))))))

; find and discard first answer, drop all primes before and including the first number of the first answer

(search (cdr (memq (car (search primes)) primes)))


  
