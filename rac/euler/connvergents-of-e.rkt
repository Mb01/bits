#lang racket

(require "utils.rkt")

;; problem statement not amenable to pasting

;; see https://projecteuler.net/problem=65

;; The first ten terms in the sequence of convergents for e are:
;; 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...

;; so we have a pattern we can break down 1 2 1, 1 4 1, 1 6 1, ...
;; if we enumerate the pattern like this  0 1 2  3 4 5  6 7 8

;; then,
;; for mod3 n = 0 or 2, 1
;; for mod3 n = 1, (* 2 (/ (+ n 2) 3))

(define (addend-at depth)
  (if (member (modulo depth 3) '(0 2))
      1
      (* 2 (/ (+ depth 2) 3))))

(define (continued depth)
  (define stop 99) ;; in this world, the 100th number is 99
  (cond
    [(= depth stop) 0]
    [else (/ (+ (addend-at depth) (continued (add1 depth))))]))
    
(define (solve)
  (+ 2 (continued 0)))

(define (sum-digits n)
  (apply + (integer->list n)))

(sum-digits (numerator (solve)))
