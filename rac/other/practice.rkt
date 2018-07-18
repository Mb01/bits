#lang racket

(define (cons-each x li)
  (map (curry cons x) li))

(define (combinations li)
  (cond
    [(null? li) '(())]
    [else
     (append
      (cons-each (car li) (combinations (cdr li)))
      (combinations (cdr li)))]))

(define (one-others li)
  ;; the idea for permutations
  (map (λ (x) (list x (remove x li))) li))

(define (permutations li)
  (cond
    [(null? li) '(())]
    [else
     (append-map
      (λ (x) (cons-each x (permutations (remove x li)))) li)]))
