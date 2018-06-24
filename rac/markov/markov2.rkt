#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note: saving to random hash tables doesn't seem to reduce times

;; I'm thinking the problem is actually cache misses when we get over a
;; certain data size

(define input-file "complete_shakespeare.txt")
;; around 500,000 it starts to slow down 
;; two million seems to be about the limit for computation during
;; a coffee break
;; markov-generate-text tends to move towards later in the text
;; the longer the input, the less chance of running into the end

(define read-n-chars 100000)

;; adds item to list for key, initializes list for key if key not in ht
(define (update ht key item); -> alters hash table
  (hash-update! ht key (curry cons item) '()))

;;choose a random element from a list
(define (choose-random li); -> el, that is, s
  (list-ref li (random (length li))))

;; tries random hash tables from list of hash tables until a key is found
(define (find-random-table-with-key li-ht key)
  (letrec
      ; we need helper to have no arguments,
      ; hash-ref calls the function given as last argument
      ; in case of a key error
      ([helper (lambda (); try a random table first
                     (hash-ref (choose-random li-ht) key helper))])
    (car (helper))))

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

(define (generate-markov-chain ht li key-length); -> mutates hash table list "ht"
  (cond
    ((< (length li) (add1 key-length))
        (update (choose-random ht) (take li key-length) 'EOF))
    (else
     (let ([key-words (take li key-length)]
           [value-word (car (drop li key-length))])
       (update (choose-random ht) key-words value-word)
       (generate-markov-chain ht (cdr li) key-length)))))

;; to improve times and avoid dead ends, we're going to use multiple hash tables
(define (markov-generate-text ht n-words n-key key); -> (s1 s2 s3...)
  ;; ht is now a list of hash tables
  ;; n-words how many words to generate
  ;; n-key, how long is the key
  ;; last-n, what were last n words for the key
  (cond
    ((zero? n-words) '())
    ;; cons the chosen word onto the list and
    ;; recurse with updated last n
    (else
     (let* ([next  (find-random-table-with-key ht key)]
            [new-key (append (cdr key) (list next))])
       (if (symbol? next)
           '("EOF")
           (cons next (markov-generate-text ht (sub1 n-words) n-key new-key)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; driver

(define lead-length 3)

; hack out a list of hash rables
(define ht (map (lambda (x) (make-hash)) (range 5)))

(define text-li (into-words (read-string read-n-chars in)))

(generate-markov-chain ht text-li 3)
;choose three words in the text that are not near the end
(display (fix-punctuation (string-join (markov-generate-text ht 10000 lead-length '("," "\n" "And")))))


