#lang racket

(define (combinations lst)
  (if (null? lst)
      '(())
      (let* (
             [withouts (combinations (cdr lst))]
             [withs (map (lambda (c) (cons (car lst) c)) withouts)])
        (append withs withouts))))

; Test the function with an input list
(define my-list '(1 2 3))
(displayln (combinations my-list))
