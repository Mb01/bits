#lang racket

(define (cons-each x li)
  (map (curry cons x) li))

(define (combinations li)
  (if (null? li) '(())
      (append
       (cons-each (car li) (combinations (cdr li)))
       (combinations (cdr li)))))

(define (one-others li)
  ;; the idea for permutations
  (map (λ (x) (list x (remove x li))) li))


;; for each el, cons it to permutations achieved by
;; calling permutations without it
;; append these lists together
(define (permutations li) ; list-> list of list
  (if (null? li) '(())
      (append-map
       (λ (el)
         (cons-each el (permutations (remove el li)))) li)))
