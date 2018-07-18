#lang racket

(define (cons* x li)
  (map (curry cons x) li))

(define (combinations li)
  (if (null? li) '(())
      (append
       (cons* (car li) (combinations (cdr li)))
       (combinations (cdr li)))))

;; for each el, cons it to permutations received by
;; calling permutations with el remoced
;; append these lists together
(define (permutations li) ; list-> list of list
  (if (null? li) '(())
      (append-map
       (λ (el)
         (cons* el (permutations (remove el li)))) li)))

(define (one-others li)
  ;; the idea for permutations
  (map (λ (x) (list x (remove x li))) li))

(define (one-others-flat li)
  (map (λ (x) (cons x (remove x li))) li))

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define (palindrome? s)
  (equal? s (string-reverse s))); wastes time checking last half
