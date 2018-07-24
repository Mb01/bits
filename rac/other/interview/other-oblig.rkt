#lang racket

;;check two strings anagrams

(define (anagrams? s1 s2)
  (define (sort-string s)
    (list->string (sort (string->list s) char<?)))
  (equal? (sort-string s1) (sort-string s2)))

(anagrams? "hello" "ehllo")

(define anagrams?*
  (lambda args
    (andmap (curry anagrams? (car args)) (cdr args))))

(anagrams?* "hello" "loehl" "ehllo")

;; reverse string using recursion is easy in lisp
(define (string-reverse s)
  (define (helper li acc)
    (if (null? li)
        acc
        (helper (cdr li) (cons (car li) acc))))
  (list->string (helper (string->list s) '())))

(string-reverse "Hello! Nice to meet you!")
      

