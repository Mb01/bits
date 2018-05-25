;;;;;;;;;;;;;;;;;;;; The following problem is from Project Euler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; PROBLEM AS STATED
#lang racket




;The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
;(i) each of the three terms are prime, and,
;(ii) each of the 4-digit numbers are permutations of one another.

;There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

;What 12-digit number do you form by concatenating the three terms in this sequence?



;;;;;;;;;;;;;;;;;;;; https://projecteuler.net/problem=49
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; increases by fixed amount, the most this can be is (4499+4499+1000)
; numbers are permutations of another

(define (prime? n)
  (define (inner n i); = has-a-factor?
    (cond
      ; end search negative when i * i is bigger than n
      ((> i (/ n i)) #f)
      ; found a factor
      ((= (modulo n i) 0) #t)
      ; keep looking
      (else (inner n (+ i 2)))))
  ; is 2 or neither divisible by 2 nor divisible by an odd number 
  (or (= n 2) (nor (inner n 3) (= (modulo n 2) 0))))

                                                      
; at least comes up with correct amount of primes under 1,000,000
(define primes-li (filter prime?  (range 1000 9999)))
(define primes-set (list->seteq primes-li))

(define (primes-set-member? n)
  (set-member? primes-set n))

(define rotate-left
  (lambda (li)
    (letrec (
             [inner
              (lambda (li carli)
                (cond ((null? li) (list carli))
                      (else (cons (car li) (inner (cdr li) carli)))))])
      (inner (cdr li) (car li)))))

(define add-to-each; cons an (el)ement to the front of each list in a (li)st; yet-another example of a missed opportunity to use map
              (lambda (el li)
                (cond
                  ((null? li) '())
                  (else (cons (cons el (car li)) (add-to-each el (cdr li)))))))

(define permutations
  (lambda (li depth original-length)
    ; if we are as deep as the original length (counting from 0), then we have considered all elements at start
    ; therefore, we are finished
    (cond
      ((= depth original-length) '(()))
      ; an element can either be at the start of a permutation or not
      (else (append 
             ; case in which at start, then we find all the permutations without that element
             (add-to-each (car li) (permutations (cdr li) 0 (sub1 original-length)))
             ; or not, we will rotate the list and set a different first element
             (permutations (rotate-left li) (add1 depth) original-length))))))

(define (number->list n acc)
  (let ([rem  (remainder n 10)])
    (if (= n 0)
        acc
        (number->list (/ (- n rem) 10) (cons rem acc)))))

(define (list->number li)
  (letrec ([inner
            (lambda (li string-acc)
              (cond
                ((null? li) (string->number string-acc))
                (else (inner (cdr li) (string-append string-acc (number->string (car li)))))))])
    (inner li "")))

; lets see if permutations having 
(define (three-perms-prime? n)
  (let ([perms-of-n (map list->number (permutations (number->list n empty) 0 (length (number->list n empty))))])
    perms-of-n))

(three-perms-prime? 675)
  

