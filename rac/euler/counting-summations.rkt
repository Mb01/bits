#lang racket

;; It is possible to write five as a sum in exactly six different ways:

;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1

;; How many different ways can one hundred be written as a sum of at least two positive integers?


;; I know there is a dynamic programming solution to this
;; but can we actually generate all of the ways

;; 2 (1) -> '((1 1))
;; 3 (2) -> '((2 1) (1 1 1)) ; so we can 
;; 4 (4) -> '((3 1) (2 2) (2 1 1) (1 1 1 1))
;; so we have four and 
;; 1 plus partitions of 3
;; 2 plus partitions of 2
;; 3 plus partitions of 1
;; then 3 is 2 and partitions of 2 and partitions of 1
;; and 2 is 2 and partitions of 1

;; but we'll have repeats out of order?  like this
;; '((1))
;; '((2) (1 1))
;; '((3) (2 1) (1 2) (1 1 1))
;; '((4) -> (3 1) (2 2) (2 1 1) -> (1 3)   -> (1 2 1) ->(1 1 2) (1 1 1 1))

;; filter out inversions?

(require "utils.rkt") ; for memoize use whatever

(define (cons-all n li)
  (map (λ (x) (cons n x)) li))

(define (ap-ap-map func li)
  (apply append (map func li)))

(define (not-inversion? li) ; only works for positive numbers!
  (define (inversion? last li)
    (cond
      ((null? li) #f); no inversion found
      ((< last (car li)) #t) ; found inversion
      (else (inversion? (car li) (cdr li)))))
  (if (null? li) #t
      (not (inversion? (car li) (cdr li)))))

(define partition
  (memoize
   (λ (n)
     (displayln n)
     (if (= n 1)
         (list (list 1))
         (cons (list n) (filter not-inversion? (ap-ap-map (lambda (x) (cons-all (- n x) (partition x))) (range 1 n))))))))

;; for example
;; (map (λ (x) (partition x)) (range 1 60))

;; and it starts to slow down from there
;; probably to many things to filter through and append etc...

;; lets see if we can generate them lexicographically
;; so we should have some order they are in
;; like this

;; 5 -> 4 1 -> 3 2 -> 3 1 1 -> 2 2 1 -> 2 1 1 1 -> 1 1 1 1

;; consider 6

;; 6 -> 5 1 -> 4 2 -> 4 1 1 -> 3 3 -> 3 2 1 -> 3 1 1 1 -> 2 2 2 -> 2 2 1 1 -> 2 1 1 1 1 -> 1 1 1 1 1 1

;; so we must break down the rightmost non-one number,
;; when we build it again, we can't use a bigger number than before

(define (next-partition li)
  (cond
    ((or (null? (cdr li)) (= 1 (cadr li))) (append (list (- (car li) 1) 1) (cdr li)))
    (else (cons (car li) (next-partition (cdr li))))))

(define (partitions2 li) ; gimme '(n)
  (if (= (car li) 1)
      li
      (cons li (partitions2 (next-partition li)))))

;; (partitions2 '(6)) -> '((6) (5 1) (4 1 1) (3 1 1 1) (2 1 1 1 1) 1 1 1 1 1 1)  d'oh!!
;; I forgot about the building the number up again without using a bigger number

;; to do this, we move one of the numbers right and add them together, it should be simple enough

(define (sum-to-n-using-x n x)
  (cond
    [(zero? n) '()]
    [(< n x) (list n)]
    [else (cons x (sum-to-n-using-x (- n x) x))]))

(define (sum-ones li)
  (define (helper li acc-n)
    (cond
      [(null? li)  acc-n]
      [(= (car li) 1) (helper (cdr li) (add1 acc-n))]
      [else (cons (car li) (helper (cdr li) acc-n))]))
  (helper li 0))

(define (contains-one li) ; we probably don't need this
  (cond
    [(null? li) #f]
    [(= 1 (car li)) #t]
    [else (contains-one (cdr li))]))

;; 6 -> 5 1 -> 4 2 -> 4 1 1 -> 3 3 -> 3 2 1 -> 3 1 1 1 -> 2 2 2 -> 2 2 1 1 -> 2 1 1 1 1 -> 1 1 1 1 1 1

(define (move-right li)
  (cond
    [(= 1 (car li)) #f]
    ;; if there is nothing after this point, then we need to make that a one
    [(null? (cdr li)) (list (sub1 (car li)) 1)]
    ;; if the nex number is a one, we need to rebuild (car li) + (sum ones)  using only (sub1 car li)
    [(= 1 (cadr li)) (sum-to-n-using-x (+ (car li) (sum-ones (cdr li))) (sub1 (car li)))]
    [else (cons (car li) (move-right (cdr li)))]))

(define (partitions-3 li) ; gimme '(n)
  (let ([next (move-right li)])
    (if next
        (cons next (partitions-3 next))
        (list))))

; use tail recursion to speed things up, also don't generate the list
(define (partitions-count li acc-n)
    (let ([next (move-right li)])
    (if next
        (partitions-count next (add1 acc-n))
        acc-n)))

(partitions-count '(100) 0)
