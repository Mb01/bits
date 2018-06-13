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

Find the sum of all 0 to 9 pandigital numbers with this property.
|#

#lang racket

(require "utils.rkt")

; one approach is brute force all permutations
; but its too slow
(define start '(9 8 7 6 5 4 3 2 1 0)) ; we'll go backwards to avoid losing the zero early

(define div-list '(2 3 5 7 11 13 17))

(define (drop1 li)
  (drop li 1))

(define (take3 li)
  (if (< (length li) 3)
      (take3 (cons 0 li)); this makes me happy
      (take li 3)))

(define (divisible? li x)
  (= (modulo (list->integer li) x) 0))

(define (has-property? li)
  (letrec
      (
       [inner
        (lambda (li div-list)
          (cond
            ((null? div-list) #t)
            ((not (divisible? (take3 li) (car div-list))) #f)
            (else (inner (drop1 li) (cdr div-list)))))])
    (inner (drop1 li) div-list)))


(define (search li)
    (let ([next (integer->list (prev-permutation (list->integer li)))])
  (cond
   ((has-property? li))
   ((cons (list->integer li) (search next)))
   (else
    (search next)))))


; ok, so what are we left with
; another approach is to build the numbers three digits at a time from the left


(define start2 (set '1 2 3 4 5 6 7 8 9 0))

(define (n-choose-3 remaining-list)
  (letrec
      (
       [inner
        (lambda (li acc)
          (cond
            ((= 3 (length acc)) (list acc))
            ((null? li) '())
            ; choose or not choose first number
            (else
             (append
              (inner (cdr li) (cons (car li) acc))
              (inner (cdr li) acc)))))])
    (inner (qsort remaining-list) '())))

(define (all-3-permutes remaining-set)
  (letrec
      ([do-one
        (lambda (perm)
          (let ([next (prev-permutation (list->integer perm))])
          (if next
              (cons (integer->list next) (do-one (integer->list next)))
              (list))))])
    (apply append (map do-one (n-choose-3 (set->list remaining-set))))))

;(define (build remaining-set div-list acc)

; well we could go down that route but I have a better idea
; let's descend into the tree with pruning

(define (slice li from to)
  (letrec ([inner
            (lambda (count acc)
              if (= count to)

(define (build remaining acc) ; the ugly way
  (cond 
    (or
     (and (= length acc 4) (not  
