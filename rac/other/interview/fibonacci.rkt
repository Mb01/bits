#lang racket

;; obligatory
(define (fibonacci n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

; see you in the next life
; (map fibonacci (range 1000))

(define (memoize func); -> wrapped function
  (let ([ht (make-hash)])
    (lambda args
      (unless (hash-has-key? ht args)
        (hash-set! ht args (apply func args)))
      (hash-ref ht args))))

; nope, recursive calls from within don't work
; calls go to original definition
;(map (memoize fibonacci) (range 5))

; this doesn't work either, memo-fib returns a function
;(define memo-fib (memoize fibonacci))
;(map  memo-fib (range 5)); -> function function function ...


;; to get calls from within to work, we need the inside
;; to call outside the memoized portion
(define fibonacci2
  (memoize (lambda (n)
             (cond
               ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (fibonacci2 (- n 1)) (fibonacci2 (- n 2))))))))

(map fibonacci2 (range 1000))



