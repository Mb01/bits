;The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

;Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

;NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.


; 

#lang racket

(require "utils.rkt")


;; we can check that numbers are removable from the left by building
;; towards the left, then we can build from the right, and set-intersect to get the answer

;; for example, we can set intersect numbers starting with 7 with numbers ending in 7 that are prime-trancatable

;; there are two numbers 23 and 53 that 

;; there's lots of room for refactoring in here
(define (solve)
  (letrec (
           [map-it
            (lambda (func1 func2 li used-even)
              ;(display (list->number li)) (newline)
              (append (func1 (func2 1 li) used-even)
                      (func1 (func2 3 li) used-even)
                      (func1 (func2 7 li) used-even)
                      (func1 (func2 9 li) used-even)

                      ; only in middle position so we can stop if used twice
                      ;(func1 (func2 0 li) (add1 used-even)) ; never
                      (func1 (func2 2 li) (add1 used-even)) ; only in middle position
                      (func1 (func2 4 li) (add1 used-even)) ; only in middle position
                      (func1 (func2 5 li) (add1 used-even)) ; only in middle position
                      (func1 (func2 6 li) (add1 used-even)) ; only in middle position
                      ;(func1 (func2 8 li) (add1 used-even)) ; never
                      ))] ; make me pretty?
           [appender
            (lambda (el li)
              (append li (list el)))]
           
           [from-left
            (lambda (li used-even)
              (if
               (and (prime? (list->number li)) (< used-even 2))
               (cons (list->number li) (map-it from-left cons li used-even))
               '() ))]
           
           [from-right
            (lambda (li used-even)
              (if
               (and (prime? (list->number li)) (< used-even 2))
               (cons (list->number li) (map-it from-right appender li used-even))
               '() ))])
    
    
    (append 
            (set-intersect (from-left '(3) 0) (from-right '(3) 0)) ; can be 3x3
            (set-intersect (from-left '(7) 0) (from-right '(7) 0)) ; can be 7x7

            (set-intersect (from-left '(3) 0) (from-right '(7) 0)) ; can be 7x3
            (set-intersect (from-left '(7) 0) (from-right '(3) 0)) ; can be 3x7

            (set-intersect (from-left '(3) 0) (from-right '(5) 0)) ; can be 5x3
            (set-intersect (from-left '(3) 0) (from-right '(2) 0)) ; can be 2x3
            
            
            )))
            

(define answer (filter (lambda (x) (< 10 x)) (solve)))

(display answer)
(newline)
(apply + answer)
