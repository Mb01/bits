#lang racket

(define (empty-string? s)
  (zero? (string-length s)))

;; generate powerset of string
(define (powerset li) ; "" -> '(()); "b" -> '((b), ()); "ab" -> '((a b), (a), (b), ())
  (cond
    ((empty? li) (list li))
    (else
     (let ([first-el (car li)])
     (letrec
         ([add-first-element
           (λ (second-el)
             (cons first-el second-el))])
       (append
        ; a set can either have something
        (map add-first-element (powerset (cdr li)))
        ; or not
        (powerset (cdr li))))))))

; call powerset with list of chars, map answer back to strings, print each string separated by a space, except for last which is empty
(for ([i (map list->string (powerset (string->list "abcd")))])
  (print i) (unless (empty-string? i) (display " ")))
