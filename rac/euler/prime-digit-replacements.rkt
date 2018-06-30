#lang racket

#|
 By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

 By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
|#

;; it specifies "part" of the number so not all of the number, and a multidigit number with all repeats is divisible by 11 or 111 etc.

;; ok there are ten possible digits that can be replaced 0-9

;; do we keep trying to add primes to families? seems like there could be an awful lot of families

;; let's enumerate some of the families

;; two digits
;; *1 (*2 note only one) *3 (*4 none) (*5 only one) (*6 none) (*7) (*8 none) *9
;; 0* 1* 2* 3* 4* 5* 6* 7* 8* 9*

;; ab* a*b *ab (a and b could be equal)

;; a** *a* **a

;;let's try working out some of the three digit families to see how we can approach the problem
;; there's probably some powerful pattern matching way of doing this

(require racket/match)
(require "utils.rkt")

;;;;;;;;;;;;;;;;
;; first attempt
;;;;;;;;;;;;;;;;

(define (replace-part pattern n) ; (replace-part '(1 3 '*) 0)

  (define (individual-replacement)
    (map (λ (x) (if (eq? x '*) n x)) pattern))
  
  (cond
    ((= n 10) '())
    (else (cons (individual-replacement) (replace-part pattern (add1 n))))))

(define pattern (list 1 3 '*))

;(filter prime? (map list->integer (replace-part pattern 0)))

;; now to generate a list of patterns
;; we can use range

(define (list-insert li pos to-insert)
  (cond
    [(and (null? li) (= pos 0)) (list to-insert)]
    [(null? li) '()]
    [(zero? pos) (cons to-insert (list-insert li (sub1 pos) to-insert))]
    [else (cons (car li) (list-insert (cdr li) (sub1 pos) to-insert))]))

;; (replace-part (list-insert '(1 2) 1 '*) 0) 
;->
#;'((1 0 2)
    (1 1 2)
    (1 2 2)
    (1 3 2)
    (1 4 2)
    (1 5 2)
    (1 6 2)
    (1 7 2)
    (1 8 2)
    (1 9 2))

(define (insert-every-position li to-insert)
  (map (λ (x) (replace-part (list-insert li x to-insert) 0)) (range 0 (add1 (length li)))))

;(insert-every-position '(1 2) '*)
#;'((
   (0 1 2)
   (1 1 2)
   (2 1 2)
   (3 1 2)
   (4 1 2)
   (5 1 2)
   (6 1 2)
   (7 1 2)
   (8 1 2)
   (9 1 2))
  ((1 0 2)
   (1 1 2)
   (1 2 2)
   (1 3 2)
   (1 4 2)
   (1 5 2)
   (1 6 2)
   (1 7 2)
   (1 8 2)
   (1 9 2))
  ((1 2 0)
   (1 2 1)
   (1 2 2)
   (1 2 3)
   (1 2 4)
   (1 2 5)
   (1 2 6)
   (1 2 7)
   (1 2 8)
   (1 2 9)))


(define 10to99 (map integer->list (range 10 100)))

(define big-list (apply append (map (λ (x) (insert-every-position x '*)) 10to99)))

(define prime-families (map (λ (x) (filter prime? (map (λ (y) (list->integer y)) x))) big-list))
; that actually worked

;;(filter (lambda (x) (= (length x) 8)) prime-families); -> nope, not even wen the range is 100000

;;;;;;;;;;
;; second attempt

;; generate a long list of primes

(define primes (filter prime? (range 1000000)))

;; first we'll find a "family" by hand, this will give us a feeling of how to build a general attack
;; let's try three digit numbers ending in one, the first two numbers need to be the same number

(define (ends-with-x? x n)
  (= x (last (integer->list n))))

(define (doubles-at-start n)
  (match-let ([(list a b c d) (integer->list n)])
    (= a b)))
    
;(filter doubles-at-start (filter (λ (li) (= (length (integer->list li)) 4)) (filter (curry ends-with-x? 1) primes)))

; this doesn't seem to be particularly fast from a human work perspective

;(define (flexible-doubles pattern n)
;  (match-let ([pattern (integer->list n)])
;    (= a b)))

;(flexible-doubles (list d a b c) 1442) -> alas, this doesn't work

;; maybe we can do what we did before but this time, instead of range, use primes
;; no, I see a very large problem with that, such as 17 is prime but **7 is not always prime

;; we might be able to search for repeating digits

;; 1121 becomes rr2r 2222 becomes rrrr 3323 becomes rr2r
;; maybe a regex is the solution after all

;; (regexp-match? #px"(.)\\1\\d\\1" (number->string 1121)); -> #t

;; let's see, how bad is that
;; for a four digit number we can have
;; should be pascal's triangle (summing to powers of two)

;; 1111                          1 1  
;; 111* 11*1 1*11 *111           4 3 1 
;; 11** 1*1* 1**1 *11* *1*1 **11 6 3 2 1
;; ***1 **1* *1** ***1           4 1 1
;; ****                          1

;; that's not too bad for even six digit numbers
;; note we don't need the first and last groups

;; then we can get rid of all the repeated characters looking back at the query
;; **1* -> 1
;; and count how repeats of each number there are
;; when we see eight repeats, we get where that came from
;; so we'll have to keep saving information so we can get back without having to calculate where we came from


;;;;;;;;
;;third and final attempt

;; generate all the key patterns, TODO refactor
(define (make-super-families family-length)

  (define (append-all s li)
    (map (λ (el) (string-append s el)) li))

  (define (comb-key len)
    (cond
      ((zero? len) (list""))
      (else (append (append-all "*" (comb-key (sub1 len)))
                    (append-all "\\d" (comb-key (sub1 len)))))))

  (define comb-key-list (comb-key family-length))

  ;; the following functions make this (.)\\1\\d\\1

  ;; (.)*\\d* -> (.)\\1\\d\\1
  (define (back-referize s)
    (string-replace s "*" "\\1"))

  (define (capturing-groupify s)
    (back-referize (string-replace s "*" "(.)" #:all? #f)))

  (define (delimify s)
    (capturing-groupify (string-append "^" s "$")))
  ;; now we can find all the super families e.g. of **1* **2* **3* etc

  (define (make-search len)
    (map delimify comb-key-list))

  ;; now for the wee complicated part of combining all the primes with all the regexes

  (define search-list (make-search family-length))

  (define primes-strings (map number->string primes))

  (define (curiable-matcher reg prim-s)
    (regexp-match? reg prim-s))

  (define (get-matcher reg)
    (curry curiable-matcher (pregexp reg)))

  (define query-funcs (map get-matcher search-list))
  
  (define (matches matcher)
    (filter matcher primes-strings))

  (define mat (map (λ (x y) (list y (matches x))) query-funcs comb-key-list))
  mat)

;; we need bigger primes
(define families (drop-right (cdr (make-super-families 6)) 1))

;; next step, we need to eliminate the numbers according the comb-key

(define (shorten family)
  (let ([family (car (cdr family))]
        [query (string-replace (car family) "\\" "")])

    (define (shorten-li fmember query)
      (cond
        ((null? fmember) '())
        ((equal? (car query) #\*) (shorten-li (cdr fmember) (cdr query)))
        (else (cons (car fmember) (shorten-li (cdr fmember) (cdr query))))))

    (define (call-shorten fmember)
      (string->number (list->string (shorten-li (string->list fmember) (string->list query)))))
    
    (cons query (map call-shorten family))))

(define shortened-families (map shorten families))

;;roll your own hash count
(define (hash-count li)
  (let ([ht (hash)])

    (define (helper li ht)
      (cond
        ((null? li) ht)
        (else
         (if (hash-has-key? ht (car li))
             (helper (cdr li) (hash-set ht (car li) (add1 (hash-ref ht (car li)))))
             (helper (cdr li) (hash-set ht (car li) 1))))))
    
    (helper li ht)))

(define counts (map hash-count shortened-families))

;; this looks in the count-hash-table of a family for a value of 8 which we are looking for
(define (search-answer cnt)
  (member 8 (hash-values cnt)))

(define rough-answer (filter identity (map (λ (cnt fam) (if (search-answer cnt) (list cnt fam) #f)) counts shortened-families)))

;; ->
#;'((#hash((1 . 5)   (131 . 3)   (61 . 5)   (163 . 4)   (29 . 3)   (37 . 3)   (41 . 3)   (167 . 3)   (191 . 3)   (67 . 4)   (71 . 2)   (193 . 3)   (221 . 2)   (233 . 8) ;; theres the answer ...   (451 . 4)   ("*d*d*d" . 1) ;; and the mask used to generate it
                     )))


                    
