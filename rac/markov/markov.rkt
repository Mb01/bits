#lang racket

(define input-file "complete_shakespeare.txt")
;; around 500,000 it starts to slow down 
;; two million seems to be about the limit for computation during
;; a coffee break
;; markov-generate-text tends to move towards later in the text
;; the longer the input, the less chance of running into the end
;; TODO: handle reaching the end more gracefully
(define read-n-chars 1000000)

(define in (open-input-file input-file))

;; adds item to list for key, initializes list for key if key not in ht
(define (update ht key item); -> alters hash table
  (hash-update! ht key (curry cons item) '()))

;;choose a random element from a list
(define (choose-random li); -> el, that is, s
  (list-ref li (random (length li))))

;; make sure no space before punctuation
(define (fix-punctuation s); -> s
  (regexp-replace* #rx" ([.,:;?!])" s "\\1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prep data 

(define (into-words s); -> li
  (define (replace-punc s); adds space before punctuation so we can split by space
    (regexp-replace* #rx"([.,:;?!])" s " \\1"))
  (define (remove-extra-white s); -> s
    (regexp-replace* #rx"[ ]+" (regexp-replace* #rx"([\n\t])+" s " \\1  ") " "))
  (define (remove-numbers s)
    (regexp-replace* #rx"[0-9]" s " "))
  (define (split-up s) ; -> li
    (filter (lambda (x) (not (equal? x ""))) (regexp-split #rx"[ ]" s)))
  (split-up (remove-extra-white (replace-punc (remove-numbers s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate a markov chain

(define (generate-markov-chain ht li key-length); -> alters hash table "ht"
  (cond
    ((< (length li) (add1 key-length))
     (update ht (take li key-length) 'EOF))
    (else
     (let ([key-words (take li key-length)]
           [value-word (car (drop li key-length))])
       (update ht key-words value-word)
       (generate-markov-chain ht (cdr li) key-length)))))

(define (markov-generate-text ht n-words key); -> (word1 word2 word3...)
  (cond
    ((zero? n-words) '())
    (else
     (let* ([next (choose-random (hash-ref ht key))]
            [new-key (append (cdr key) (list next))])
       (cons next (markov-generate-text ht (sub1 n-words) new-key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; driver

(define ht (make-hash))
(define key-length 3)
(define n-words 10000)
(define taste (into-words (read-string read-n-chars in)))

(generate-markov-chain ht taste key-length)

;; choose three words in the text that are not near the end
(display (fix-punctuation (string-join (markov-generate-text ht n-words '("Will" "be" "a")))))


