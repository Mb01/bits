#lang racket

;; obligatory

(define (fibonacci n)
  (cond
    ((= 0 n) 0)
    ((= 1 n) 1)
    (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(map fibonacci (range 10))

