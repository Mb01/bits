#lang racket

(define (space)
  (display " "))

(define (memoize func)
  (let ([result-ht (make-hash)])
    (lambda args ; this is the rest-id pattern
      (when (not (hash-has-key? result-ht args))
        (hash-set! result-ht args (apply func args)))
      (hash-ref result-ht args))))

; all permutations of payments
(define (coin-problem-perms coins amount) ; gimme coins ascending
  (letrec
      ([look-table
        (memoize (lambda (amount)
                   (letrec
                       ([mapper
                         (lambda (coin)
                           (displayln (list amount coin (look-table (- amount coin))))
                           (look-table (- amount coin)))])
                     (cond
                       ((< amount 0) 0)
                       ((= amount 0) 1)
                       (else (apply + (map mapper coins)))))))])
    (look-table amount)))

; unique sets of coins
; we need to build up from the bottom else brute force ; oh well
(define coin-problem-partition ; gimme coins descending
  (memoize
   (lambda (coins amount)
     (displayln (list coins amount))
     (cond
       ((null? coins) 0)
       (else
        (let ([amount-after (- amount (car coins))])
          (if
            (= amount-after 0)
            (+ 1 (coin-problem-partition (cdr coins) amount)) ; perfect, we reached the sum with that coin, but try others
            (if (> amount-after 0)
                (+ (coin-problem-partition coins amount-after) (coin-problem-partition (cdr coins) amount)) ; try both new amount and different coins
                (coin-problem-partition (cdr coins) amount))))))))); else just try different coins

(define (coin-problem-least-coins coins amount) ; gimme descending
  (displayln (list coins amount))
  (cond
    ((= amount 0)
     0) ; we don't have to pay anything 
    ((null? coins)
     +inf.0) ; we'll never get there
    ((not (< (- amount (car coins)) 0))
     (add1 (coin-problem-least-coins coins (- amount (car coins))))); use coin, keep using that coin on the new amount
    (else
     (coin-problem-least-coins (cdr coins) amount)))); keep trying current amount with different coins



;(define (even-partitions))


;(define (knapsack)) 
  
