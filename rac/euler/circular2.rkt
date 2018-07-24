#lang racket
;;;;;;;;;;;;;;;;;;;; The following problem is from Project Euler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; PROBLEM AS STATED
;The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

;There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

;How many circular primes are there below one million?

; we'll just do them one by one and avoid state
; silly me this only took a few minutes to code when I avoided mutable sets
; and it was faster!

(define upto 1000000)

; special version
(define (prime? n)
  (define (prime?-helper n i)
    (cond
      ((> i (/ n i)) #f)
      ((= (modulo n i) 0) #t)
      (else (prime?-helper n (+ i 2)))))
  (or (= n 2) (nor (prime?-helper n 3) (= (modulo n 2) 0))))

; a mutable set having primes to "upto"
; at least comes up with correct amount of primes under 1,000,000
(define primes-li (filter prime?  (range 2 upto)))
(define primes-set (list->seteq primes-li))

(define rotate-left
  (lambda (li)
    (letrec (
             [inner
              (lambda (li carli)
                (cond ((null? li) (list carli))
                      (else (cons (car li) (inner (cdr li) carli)))))])
      (inner (cdr li) (car li)))))

(define (circs n)

  (define (circs-inner n-li depth)
    (cond
      ((= depth (length n-li)) '())
      (else (cons (string->number (list->string n-li)) (circs-inner (rotate-left n-li) (add1 depth))))))
  
  (circs-inner (string->list (number->string n)) 0))

(define (circular-prime? n)
  (letrec (
           [inner
            (lambda (circ)
              (cond
                ((null? circ) #t)
                ((set-member? primes-set (car circ)) (inner (cdr circ)))
                (else #f)))])
    (inner (circs n))))

(length (filter circular-prime? primes-li))

