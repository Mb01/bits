#lang racket

; we find best way of ending at i
; if best way of ending at i is the best way to end
; then we have our answer

(define (subarray li)
  (define (helper li here-max current-max)
    (if (null? li)
        current-max
        (let ([here-max (max (car li) (+ here-max (car li)))])
          (helper
           (cdr li)
           here-max
           (max current-max
                here-max)))))
  (helper li 0 0))

(subarray '(-2 -5 6 -2 -3 1 5 -6)) ; 7
