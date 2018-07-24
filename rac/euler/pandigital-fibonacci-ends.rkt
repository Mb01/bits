#lang racket

;; The Fibonacci sequence is defined by the recurrence relation:

;;     Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

;; It turns out that F541, which contains 113 digits, is the first Fibonacci number for which the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order). And F2749, which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.

;; Given that Fk is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.

(require "utils.rkt")
(require racket/generator)

(define one-billion 1000000000)

;; special versions for this program
;; we're going to remove a key three lower than the current argument
(define (memoize func) ; ->memoized function
  (let ([result-ht (make-hash)])
    (lambda (arg)
      (when (not (hash-has-key? result-ht arg))
        (hash-set! result-ht arg (func arg)))
      ;; this happens after the hash is set so no worries about previous
      ;; uncomputed values not having something they need
      (when (hash-has-key? result-ht (- arg 3))
        (hash-remove! result-ht (- arg 3)))
      (hash-ref result-ht arg))))

;; I'm conjecturing that small numbers never catch
;; up with growth
;; so we can just round them off at a certain rate

(define fibonacci
  (memoize
   (lambda (n)
     (cond
       [(= n 1) 1]
       [(= n 2) 1]
       [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))))

;; instead of memoize or generators, the fastest way seems to be just
;; building a giant list and looking through it
;; no not enough memory

#;(define (fibs-to-n n)
  (define (interior at last1 last2 acc)
    (let ([answer (+ last1 last2)]) 
      (if (< at n)
          (interior (add1 at) answer last1 (cons answer acc))
          acc)))
  (interior 3 1 1 '(1 1)))

;; these are too slow
#;(define (ends-pandigital19? n)
  (and (> n 99999999)
  (pandigital19? (list->integer (take-right (integer->list n) 9)))))

#;(define (starts-pandigital19? n)
  (and (> n 99999999)
  (pandigital19? (list->integer (take (integer->list n) 9)))))

(define (ends-pandigital19? n)
  (and (> n 99999999)
  (pandigital19? (modulo n one-billion))))

(define (starts-pandigital19? n)
  (and (> n 99999999)
  (pandigital19? (list->integer (take (integer->list n) 9)))))


(define (search n last1 last2)
  (let ([fib  (+ last1 last2)])
    (when (zero? (modulo n 1000)) (displayln n))
    (cond
      [(and (ends-pandigital19? (modulo fib one-billion)) (starts-pandigital19? (fibonacci n)))  n]
      [else (search (add1 n) fib last1)])))

(search 3 1 1)
;; not one minute

                
     
    
