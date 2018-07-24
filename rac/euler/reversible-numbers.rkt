#lang racket



;; Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).

;; There are 120 reversible numbers below one-thousand.

;; How many reversible numbers are there below one-billion (109)?

(require "utils.rkt")

(define one-billion 1000000000) ;; actually no 9 digit reversibles

(define (reversible? n)
  (let ([reversed (list->integer (reverse (integer->list n)))])
  (cond
    [(zero? (modulo n 10)) #f] ; can't have leading zeroes
    [else (andmap odd? (integer->list (+ n reversed)))])))

(define (search n acc-n)
  (let ([acc-n (+ acc-n (if (reversible? n) 1 0))])
  (if (< n one-billion)
      (search (add1 n) acc-n)
      acc-n)))

(search 1 99999999)
;; not within one-minute

;; there's probably a way of counting how many numbers have this property

