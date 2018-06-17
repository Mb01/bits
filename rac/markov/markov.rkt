#lang racket



; adds item to list for key, initializes list for key if key not in ht
(define (update ht key item); -> alters hash table
  (hash-update! ht key (curry cons item) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prep data

(define in (open-input-file "complete_shakespeare.txt")); -> port

(define (into-words s); -> li
  (define (remove-delim s); -> s
    (regexp-replace* #rx"[\n\t ]+" s " "))
  (define (split-up s) ; -> li
    (filter (lambda (x) (not (equal? x ""))) (regexp-split #rx"[ .,:;?!]" s)))
  (split-up (remove-delim s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate a markov chain


(define ht (make-hash))

(define (generate-markov-chain ht li n-key n-value); -> alters hash table "ht"
  (cond
    ((< (length li) (+ n-key n-value)) li); -> return remaining words for checking
    (else
     (let ([key-word (take li n-key)]
           [value-words (take (drop li n-key) n-value)])
       (update ht key-word value-words)
       (generate-markov-chain ht (drop li n-key) n-key n-value)))))
  
(define taste (into-words (read-string 1000 in)))

(generate-markov-chain ht taste 1 3)
