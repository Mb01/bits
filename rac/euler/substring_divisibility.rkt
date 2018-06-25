#|
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pan-digital numbers with this property.
|#

;; ok, we know these are not amenable to brute force anyway,
;; another approach is to build the numbers from the left and back track

#lang racket

(require "utils.rkt") ; for integer->list / list->integer

(define start '(9 8 7 6 5 4 3 2 1 0)) ; we'll go backwards to avoid losing the zero early

;; added ones to beginning to check "" d1 d1d2 d1d2d3
(define div-list '(1 1 1 2 3 5 7 11 13 17))

;; returns last elements in a list, or at least three numbers when list not long enough
(define (last-three li)
  (cond
    ((< (length li) 3) '(1 2 3))
    ((= (length li) 3) li)
    (else (last-three (cdr li)))))

;; removes el from li
(define (remove-member li el)
  (cond
    ((null? li) '())
    ((= (car li) el) (remove-member (cdr li) el))
    (else (cons (car li) (remove-member (cdr li) el)))))

(define (search starting-numbers div-list)
  
  ;; numbers move from remaining to acc as used
  ;; try is number of tries and indice across remaining
  ;; div-list has the current number by which (last-three li) must be divisible
  (define (helper remaining try div-list acc)
    (cond
      ((null? remaining) (list (list->integer acc))) ; answer found
      ((= try (length remaining)) (list)) ; check if we exhausted tries
      (else ;; examine accumulator after adding value, and continue trying
       (let* (;; element to try
              [try-el (list-ref remaining try)]
              ;; accumulator with new value added
              [new-acc (append acc (list try-el))])
         (append
          (if ; we fulfill the divisibility requirement with new-acc
           (zero? (modulo (list->integer (last-three new-acc)) (car div-list)))
           ;;remove tried el, reset try number, iter div-list, update acc and recurse
           (helper (remove-member remaining try-el) 0 (cdr div-list) new-acc)
           (list))
          ;; keep trying with old accumulator
          (helper remaining (add1 try) div-list acc))))))
  (helper starting-numbers 0 div-list '()))

(apply + (search start div-list))

