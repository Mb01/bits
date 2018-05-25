#lang racket
;;;;;;;;;;;;;;;;;;;; The following problem is from Project Euler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; PROBLEM AS STATED
;The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

;There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

;How many circular primes are there below one million?

; there's a bug in here somewhere, optimization is the root of all evil

(define upto 1000000)

(define (prime? n)
  (define (prime?-helper n i)
    (cond
      ((> i (/ n i)) #f)
      ((= (modulo n i) 0) #t)
      (else (prime?-helper n (+ i 2)))))
  (or (= n 2) (nor (prime?-helper n 3) (= (modulo n 2) 0))))



; a mutable set having primes to "upto"
; at least comes up with correct amount of primes under 1,000,000
(define primes (list->mutable-seteq (filter prime?  (range 2 upto))))



(define rotate-left
  (lambda (li)
    (letrec (
             [inner
              (lambda (li carli)
                (cond ((null? li) (list carli))
                      (else (cons (car li) (inner (cdr li) carli)))))])
      (inner (cdr li) (car li)))))


(define (circulars n)

  (define (circulars-inner n-li depth)
    (cond
      ((= depth (length n-li)) '())
      (else (cons (string->number (list->string n-li)) (circulars-inner (rotate-left n-li) (add1 depth))))))
  
  (circulars-inner (string->list (number->string n)) 0))




(define (is-circular-in-set circles primes-set tested-mu-seteq counter)
  (cond
    ((null? circles) counter)
    ; find a new prime that's circular? and under 1 million
    ((and (set-member? primes-set (car circles)) (< (car circles) 1000000))
     ; side effect here
     (set-remove! primes-set (car circles))
     (set-add! tested-mu-seteq (car circles))
     (is-circular-in-set (cdr circles) primes-set tested-mu-seteq (add1 counter)))
    ; just circular
    ((set-member? primes-set (car circles))
     (set-remove! primes-set (car circles))
     (set-add! tested-mu-seteq (car circles))
     (is-circular-in-set (cdr circles) primes-set tested-mu-seteq counter))
     ; found an already tested prime? keep going
     ((set-member? tested-mu-seteq (car circles))
      (is-circular-in-set (cdr circles) primes-set tested-mu-seteq counter))
    (else 0)))


(define (solve primes-set counter)
  (if (set-empty? primes-set)
      counter
      (solve primes-set (+ counter (is-circular-in-set (circulars (set-first primes-set)) primes-set (mutable-seteq) 0)))))

(solve primes 0)
     
       
            
  
  
  
  
  

