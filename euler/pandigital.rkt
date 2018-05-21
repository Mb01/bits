;;;;;;;;;;;;;;;;;;;; The following problem is from Project Euler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; PROBLEM AS STATED

;We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

;The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

;Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
;HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

;;;;;;;;;;;;;;;;;;;; https://projecteuler.net/problem=32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we can see that
;(length (permutations problem-range initial-depth (length problem-range))); -> 986410

; we need to reduce the number of partitions for each permutation

; We can skip partitions that don't have a solution.

; whenever the first partition has more digits than the last number
; it can't be part of a solution

; importantly, no partitions moving the middle or last partition further right has a new solution because the numbers on the left only get larger


#lang racket

(define empty-acc '())
(define zero 0)
(define one 1)
(define two 2)
(define initial-depth 0)
(define problem-range (range 1 10)); we need to set 5 back to 10!!!!!!!!!
(define problem-range-length (length problem-range))

(define (list->number li)
  (letrec ([inner
            (lambda (li string-acc)
              (cond
                ((null? li) (string->number string-acc))
                (else (inner (cdr li) (string-append string-acc (number->string (car li)))))))])
    (inner li "")))

(define (ab=c? a b c)
  (eq? (* a b) c))

(define add-to-each; cons an (el)ement to the front of each list in a (li)st; yet-another example of a missed opportunity to use map
              (lambda (el li)
                (cond
                  ((null? li) '())
                  (else (cons (cons el (car li)) (add-to-each el (cdr li)))))))

(define rotate-left
  (lambda (li)
    (letrec (
             [inner
              (lambda (li carli)
                (cond ((null? li) (list carli))
                      (else (cons (car li) (inner (cdr li) carli)))))])
      (inner (cdr li) (car li)))))

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


(define permutes-to-evaluate 
  (filter
   (lambda (x) (= (length x) problem-range-length))
   (permutations problem-range initial-depth problem-range-length)))



; ok the brute force way to this is like this
; obviously there is some limitation on multiplication being larger than the largest number (987654321) possible
;;;;;;;;;;;;;;;;;;;partitioning
; partitions walls on left move right if possible
; otherwise move the next leftmost partition ...
; all partitions on left of moved partitions are brought back to the base position
; 11100 -> 110100 -> 10110 -> 01110 -> 11001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (partition-into-three base-li)
  (letrec (
           
           [make-one-partition ; get a slice (not including end)
            (lambda (pw1 pw2 base-li acc-li)
              (cond
                ((zero? pw2)
                 (reverse acc-li))
                ((zero? pw1)
                 (make-one-partition pw1 (sub1 pw2) (cdr base-li) (cons (car base-li) acc-li)))
                (else
                 (make-one-partition (sub1 pw1) (sub1 pw2) (cdr base-li) acc-li))))]
           
           [make-partitions ; splits list into three ; we could do this in one pass
            (lambda (pw1 pw2)
              (list (make-one-partition zero pw1              base-li empty-acc)
                    (make-one-partition  pw1  pw2              base-li empty-acc)
                    (make-one-partition  pw2  (length base-li) base-li empty-acc)))]
           [inner
            (lambda (pw1 pw2 acc-li)
              ;(display (list pw1 pw2)) (newline)(newline)
              (let (
                    [add-acc (cons (make-partitions pw1 pw2) acc-li)])
                (cond
                  ((> pw1 (- (length base-li) pw2 1)) add-acc)
                  ((not (= (add1 pw1) pw2))              (inner (add1 pw1) pw2  add-acc))
                  ((not (= (add1 pw2) (length base-li))) (inner one (add1 pw2) add-acc))
                  (else add-acc))))])
    
    (inner one two empty-acc)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;end_partitioning

 
; get a list of all partitioned permutations
(define lists-of-partitions (map partition-into-three permutes-to-evaluate))
(define all-partitions (append* (car lists-of-partitions) (cdr lists-of-partitions)))

; keep any that satisfy the problem
(define passing-partitions
  (filter
   (lambda (x)
     (apply ab=c? (map list->number x))) all-partitions))

; get the portion representing the product that we have to sum
(define products-to-sum 
  (map list->number (map
   (lambda (x) (car (cdr (cdr x))))
   passing-partitions)))


(apply + (set->list (list->set products-to-sum)))
