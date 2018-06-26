#lang racket

;; The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

;; How many n-digit positive integers exist which are also an nth power?

;; I wonder if 1^1 2^1 3^1 etc count?

; let's take a look at some lengths of powers

(require "utils.rkt")

(define (integer-length n)
  (length (integer->list n)))

;(map (λ (x y) (printf "~a^~a = ~a len: ~a~n" x y (expt x y) (integer-length (expt x y)))) '(2 9 10 99 100 999 1000 9999 10000 99999) '(2 2 2 2 2 2 2 2 2 2))

;; 2^2 = 4              len: 1
;; 9^2 = 81             len: 2
;; 10^2 = 100           len: 3 ; too big
;; ...

;; so the length of a number raised to the power of 2 is already more than two

;(map (λ (x y) (printf "~a^~a = ~a len: ~a~n" x y (expt x y) (integer-length (expt x y)))) '(2 9 10 99 100 999 1000 9999 10000 99999) '(3 3 3 3 3 3 3 3 3 3))

;; 2^3 = 8       len: 1
;; 9^3 = 729     len: 3
;; 10^3 = 1000   len: 4 cutoff is 2 digits

;; ok, with some thought, the proper length is 4 to 9, let's show that

;(map (λ (x y) (printf "~a^~a = ~a len: ~a~n" x y (expt x y) (integer-length (expt x y)))) '(2 3 4 5 6 7 8 9 10) '(2 2 2 2 2 2 2 2 2))

;; 2^2 = 4 len: 1
;; 3^2 = 9 len: 1 ; too short
;; 4^2 = 16 len: 2
;; 5^2 = 25 len: 2
;; 6^2 = 36 len: 2
;; 7^2 = 49 len: 2
;; 8^2 = 64 len: 2
;; 9^2 = 81 len: 2
;; 10^2 = 100 len: 3 ; too large

;; let's also examine the behavior of 9
;; I really wish I had written this as a function earlier ...
;;(map (λ (x y) (printf "~a^~a = ~a len: ~a~n" x y (expt x y) (integer-length (expt x y)))) '(9 9 9 9 9 9 9 9 9 9) '(2 3 4 5 6 7 8 9 10 11))

;; 9^2 = 81 len:            2
;; 9^3 = 729 len:           3
;; 9^4 = 6561 len:          4
;; 9^5 = 59049 len:         5
;; 9^6 = 531441 len:        6
;; 9^7 = 4782969 len:       7
;; 9^8 = 43046721 len:      8
;; 9^9 = 387420489 len:     9
;; 9^10 = 3486784401 len:   10
;; 9^11 = 31381059609 len:  11

;; uh oh,

;; let's write what I've been using as a function so we can explore more numbers quickly

(define (print-expt bases powers)
  (map (λ (x y) (printf "~a^~a = ~a len: ~a~n" x y (expt x y) (integer-length (expt x y)))) bases powers)
  ;; don't return result
  (void))

(require racket/generator)

(define nine
  (infinite-generator (yield 9)))

;; (print-expt (map (lambda (x) (nine)) (range 100)) (range 100))
;; that was hideous
  
;; 9^2 = 81 len: 2
;; 9^3 = 729 len: 3
;; 9^4 = 6561 len: 4
;; 9^5 = 59049 len: 5
;; 9^6 = 531441 len: 6
;; 9^7 = 4782969 len: 7
;; 9^8 = 43046721 len: 8
;; 9^9 = 387420489 len: 9
;; 9^10 = 3486784401 len: 10
;; 9^11 = 31381059609 len: 11
;; 9^12 = 282429536481 len: 12
;; 9^13 = 2541865828329 len: 13
;; 9^14 = 22876792454961 len: 14
;; 9^15 = 205891132094649 len: 15
;; 9^16 = 1853020188851841 len: 16
;; 9^17 = 16677181699666569 len: 17
;; 9^18 = 150094635296999121 len: 18
;; 9^19 = 1350851717672992089 len: 19
;; 9^20 = 12157665459056928801 len: 20
;; 9^21 = 109418989131512359209 len: 21
;; 9^22 = 984770902183611232881 len:    21 .... ding ding ding, a winner

;; My homework, write: "DRY is good. I will write generic functions." 99 times.

(define (r100) (range 100))

(define (same-n-length-matching-r100 n)
  (map (lambda (x) n) r100)) ; d'oh, too late

;; observations:
;; once the power is passed by the length, it will stay passed
;; numbers greater than 9 always pass, so we can stop at nine
;; one is the first positive integer


;; solution


(define (search cur-base cur-power acc)
  (let* ([cur-answer (expt cur-base cur-power)]
         [cur-answer-length (integer-length cur-answer)])
    (cond
      ((> cur-base 9) acc) ; finish-line
      ((not (= cur-answer-length cur-power))
            (search (add1 cur-base) 1  acc))
      (else
       (search cur-base (add1 cur-power) (cons (list cur-base cur-power) acc))))))
      

(define answer (search 1 1 '()))
answer
(length answer)
