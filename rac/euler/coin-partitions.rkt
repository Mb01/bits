#lang racket

;; Let p(n) represent the number of different ways in which n coins can be separated into piles.
;; For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.
;; OOOOO
;; OOOO   O
;; OOO   OO
;; OOO   O   O
;; OO   OO   O
;; OO   O   O   O
;; O   O   O   O   O

;; Find the least value of n for which p(n) is divisible by one million.

(require "utils.rkt")

;; partition = combinations not permutations of sums to an integer

(define one-million 1000000)

;; first I tried the recursive algorithm

(define p1
  (memoize
   (λ (n lim) ;; lim <- biggest pile we can use
     (cond
       [(< n 0) 0]
       [(zero? n) 1]
       [else (apply +
                    (map (λ (x) (p1 (- n x) x)) (range 1 (min (add1 lim) (add1 n)))))]))))

(define (call-p n) ;; anti-pattern, should wrap p1 instead
  (p1 n n))

(define (search n)
  (displayln n)
  (if (zero? (modulo (call-p n) 1000000)) n
      (search (add1 n))))

;;(search 1) ; that takes ... forever?

;; then I tried dynamic programming

;; we can generate a table that looks like this

;;((0)
;;(ans1,1) ; for 1
;;(ans2,1 ans2,2)
;;(ans1 ans2 ans3))
;; ...

(define maxsize 1000)

(define table (make-vector maxsize))

(vector-set! table 0 (make-vector 1))

(vector-set! (vector-ref table 0) 0 1)

(define (update-table x y n)
  ;(displayln (list x y))
  (when (not (vector? (vector-ref table x)))
    (vector-set! table x (make-vector (add1 x))))
  (vector-set! (vector-ref table x) y n))

(define (refer-table x y)
  ;(displayln (list x y))
  (cond
    [(= x 0) 1]
    [(or (< x 0) (< x y)) 0]
    [else (vector-ref (vector-ref table x) y)]))

(define (build-table n lim)
  ;;(displayln (list n lim))
  (cond
    [(= n maxsize) 'exhuasted-maxsize]
    [(zero? lim) (build-table (add1 n) (add1 n))]
    [else (update-table n
                        lim
                        (modulo
                         (apply +
                                (map
                                 (λ (x)
                                   (refer-table (- n x) x)) (range 1 (add1 lim))))
                         one-million))
          
          (build-table n (sub1 lim))]))

;(build-table 1 1)
;; not even remotely fast enough

;; we'll need a number theory solution

;; ok, I've done some googling on partitions

(define gen-pents (gen-pent-n 250)) ; 250 empirically determined

;; + p(n - 1) + p(n - 2) - p(n - 5) - p(n - 7) + ... + ... - ... -
(define (formula gen-pents n acc)
  (cond
    [(< n (first gen-pents)) acc]
    [else (formula (drop gen-pents 4) n
                   (-
                    (+ acc
                       (p (- n (first  gen-pents)))
                       (p (- n (second gen-pents))))
                    (p (- n (third  gen-pents)))
                    (p (- n (fourth gen-pents)))))]))

(define p
  (memoize
   (λ (n)
     (cond
       [(< n 0) 0]
       [(zero? n) 1]
       [else (formula gen-pents n 0)]))))

(define (find n)
  (cond
    [(zero? (modulo (p n) one-million)) n]
    [else (find (add1 n))]))

;; that does it
