#lang racket
;;;;;;;;;;;;;;;;;;;; The following problem is from Project Euler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; PROBLEM AS STATED


;The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

;Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

;(Please note that the palindromic number, in either base, may not include leading zeros.)


(define (num->bins n)
  (number->string n 2))

(define (reversed-string s)
  (letrec ([reversed-list
            (lambda (acc li)
              (cond
                ((null? li) acc)
                (else (reversed-list (cons (car li) acc) (cdr li)))))])
    (list->string (reversed-list '() (string->list s) ))))


(define (palindrome? s)
  (and (string=? s (reversed-string s))
       (not (eqv? (string-ref s 0) #\0))
       (not (eqv? (string-ref (reversed-string s) 0) #\0))))

(define (double-palindrome? n)
(and
 (palindrome? (number->string n))
 (palindrome? (num->bins n))))

(foldl + 0 (filter double-palindrome? (range 1 1000000)))
 
