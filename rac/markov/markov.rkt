#lang racket

(define input-file "complete_shakespeare.txt")
;; around 500,000 it starts to slow down 
;; two million seems to be about the limit for computation during
;; a coffee break
;; markov-generate-text tends to move towards later in the text
;; the longer the input, the less chance of running into the end
;; TODO: handle reaching the end more gracefully
(define read-n-chars 300000)

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

(define in (open-input-file input-file));

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

(define (generate-markov-chain ht li n-key n-value); -> alters hash table "ht"
  (cond
    ((< (length li) (+ n-key n-value)) li); -> return remaining words for checking
    (else
     (let ([key-word (take li n-key)]
           [value-words (take (drop li n-key) n-value)])
       (update ht key-word value-words)
       (generate-markov-chain ht (cdr li) n-key n-value)))))

;; TODO: try to avoid a dead end
;; it seems that the hash table creation gets slow with increased size
;; maybe we can make more hash tables containing more of the text
;; then we can keep trying to revert to the first hash table
;; then we can reduce time and avoid dead ends
(define (markov-generate-text ht n-words n-key key); -> (s1 s2 s3...)
  ;; n-words how many words to generate
  ;; n-key, how long is the key
  ;; last-n, what were last n words for the key
  (cond
    ((zero? n-words) '())
    ;; cons the chosen word onto the list and
    ;; recurse with updated last n
    (else
     (let* ([next (choose-random (hash-ref ht key))]
            [new-key (append (cdr key) next)])
       (cons
        (car next)
        (markov-generate-text ht (sub1 n-words) n-key new-key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; driver

(define ht (make-hash))
(define lead-length 3)
(define taste (into-words (read-string read-n-chars in)))

(generate-markov-chain ht taste lead-length 1)

;; choose three words in the text that are not near the end
(display (fix-punctuation (string-join (markov-generate-text ht 10000 lead-length '("Will" "be" "a")))))


