#lang racket
#|
 It is possible to write five as a sum in exactly six different ways:

4 + 1                                
3 + 2
3 + 1 + 1
2 + 2 + 1
2 + 1 + 1 + 1
1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at least two positive integers?

note: since I'm starting to have a collection of solutions which could facilitate finding many solutions quicly
I'm discouraging would be cheaters by hiding the actual solutions as much as possible.

I know there is a dynamic programming solution to this, just like the "change problem."
It turns out generating the actual "ways to write" (hereinafter partitions) for 100 is too much for an in-memory solution.
However, I was able to iterate through all the partitions.

I was optimistic about memoizing this, it should work for "number of partitions."

2 (1) -> '((1 1))
3 (2) -> '((2 1) (1 1 1))
4 (4) -> '((3 1) (2 2) (2 1 1) (1 1 1 1))
so 4 is 
1 plus partitions of 3 
2 plus partitions of 2
3 plus partitions of 1
then 3 is 2 and partitions of 2 and partitions of 1
and 2 is 2 and 1 plus partitions of 1

 but my first attempt required "a hack" to get rid of unwanted duplicate answers,
 Perhaps I can compare how the dynamic programming solution builds up to get a hint as to where these come from?
|#

(require "utils.rkt") ; for memoize use whatever

(define (cons-all n li)
  (map (λ (x) (cons n x)) li))

(define (ap-ap-map func li)
  (apply append (map func li)))

(define (not-inversion? li) ; only works for positive numbers!
  (define (inversion? last li)
    (cond
      ((null? li) #f); no inversion found
      ((< last (car li)) #t) ; found inversion
      (else (inversion? (car li) (cdr li)))))
  (if (null? li) #t
      (not (inversion? (car li) (cdr li)))))

(define partition
  (memoize
   (λ (n)
     (displayln n)
     (if (= n 1)
         (list (list 1))
         (cons (list n) (filter not-inversion? (ap-ap-map (lambda (x) (cons-all (- n x) (partition x))) (range 1 n))))))))

;; for example
;; (map (λ (x) (partition x)) (range 1 60))

;; and it starts to slow down from there
;; probably too big for memory

;; '((1))
;; '((2) (1 1))
;; '((3) (2 1) (1 2) (1 1 1))

;; I think there is some problem because I'm getting inversions.
;; There should be some way to avoid this, but I continued along different lines to find the answer
;; after hacking on a way to filter out inversions that are duplicate answers

;; lets see if we can generate them lexicographically
;; so we should have some order they are in
;; like this

;; 5 -> 4 1 -> 3 2 -> 3 1 1 -> 2 2 1 -> 2 1 1 1 -> 1 1 1 1

;; consider 6

;; 6 -> 5 1 -> 4 2 -> 4 1 1 -> 3 3 -> 3 2 1 -> 3 1 1 1 -> 2 2 2 -> 2 2 1 1 -> 2 1 1 1 1 -> 1 1 1 1 1 1

;; so we must break down the rightmost non-one number,
;; when we build it again, we can't use a bigger number than before

(define (next-partition li)
  (cond
    ((or (null? (cdr li)) (= 1 (cadr li))) (append (list (- (car li) 1) 1) (cdr li)))
    (else (cons (car li) (next-partition (cdr li))))))

(define (partitions2 li) ; gimme '(n)
  (if (= (car li) 1)
      li
      (cons li (partitions2 (next-partition li)))))
#|
(partitions2 '(6)) -> '((6) (5 1) (4 1 1) (3 1 1 1) (2 1 1 1 1) 1 1 1 1 1 1)  d'oh!!
I forgot about the building the number up again without using a bigger number

To find next-partition,
rightmost is rightmost number that is not one.
sum-ones is sum of all ones after rightmost.
new-end is (rightmost - 1) repeated (sum-one + rightmost)/(rightmost - 1) times followed by the remainder of that division
next-partition is current-partition with rightmost replaced with new-end.

When the partition contains only ones, we can't find rightmost.
If we start with n as the first partition, and iterate until reaching the partition that 
contains only ones, then we have iterated through all of the partitions of n.


consider again 6

6 -> 5 1 -> 4 2 -> 4 1 1 -> 3 3 -> 3 2 1 -> 3 1 1 1 -> 2 2 2 -> 2 2 1 1 -> 2 1 1 1 1 -> 1 1 1 1 1 1

rightmost = 6, sum-ones = 0, rightmost - 1 = 5, remainder = 1, -> 5 1
rightmost = 5, sum-ones = 1, rightmost - 1 = 4, remainder = 2, -> 4 2
rightmost = 2, sum-ones = 0, rightmost - 1 = 1, remainder = 0, -> 4 1 1
|#

(define (sum-to-n-using-x n x)
  (cond
    [(zero? n) '()] ; append nothing for zero
    [(< n x) (list n)]; append remainder when n is too small
    [else (cons x (sum-to-n-using-x (- n x) x))])) ; add x to list, keep trying

(define (sum-ones li)
  (define (helper li acc-n)
    (cond
      [(null? li)  acc-n] ; nothing more to add
      [(= (car li) 1) (helper (cdr li) (add1 acc-n))] ; is 1, add one, keep adding
      [else (cons (car li) (helper (cdr li) acc-n))])) ; not 1, don't add yet.
  (helper li 0))

(define (move-right li)
  (cond
    [(= 1 (car li)) #f] ; if one is in left position, we can't move anything to the right, so we're finished
    ;; The next line is a small shortcut from the algorithm described above.
    ;; If rightmost is also the last element, then the remainder is always one, e.g. 5 ---(5 / 4 is 1 remainder 1)---> 4 1
    [(null? (cdr li)) (list (sub1 (car li)) 1)] 
    ;; replace rightmost with new-end as described above, rightmost is (car li)
    [(= 1 (cadr li)) (sum-to-n-using-x (+ (car li) (sum-ones (cdr li))) (sub1 (car li)))]
    ;; keep the current element in the list, continue searching for rightmost
    [else (cons (car li) (move-right (cdr li)))]))

(define (partitions-3 li) ; gimme '(n)
  (let ([next (move-right li)])
    (if next
        (cons next (partitions-3 next))
        (list))))

;; use tail recursion to speed things up, also don't generate the list
(define (partitions-count li acc-n); does not include '(n) itself!
    (let ([next (move-right li)])
    (if next
        (partitions-count next (add1 acc-n))
        acc-n)))

;; (partitions-count '(100) 0) ; this returns in "reasonable" time and correctly

;; returning now to generating the actual numbers
;; let's use proper tail recursion to see if we can improve performance
(define (partitions-4 li acc) ; gimme '(n) '()
  (let ([next (move-right li)])
    (if next
        (partitions-4 next (cons li acc)); we could cons on next to eliminate original n
        (cons li acc))))

;;(length (partitions-4 '(70) '())) ; about 70 is the limit, runs out of memory

;; We had to find the answer to actually understand why we can't generate all the numbers.

;; The number of bytes would be roughly (* n-partitions numbers-per-partition)

;; To find numbers-per-partition, I use:

(define (average-length li) ; list of lists -> integer: average length
  (define (helper helper-li running-sum)
    (cond
      [(null? helper-li) (/ running-sum (length li))]
      [(helper (cdr helper-li) (+ (length (car helper-li)) running-sum))]))
  (if (null? li)
      0
      (helper li 0)))

;;(exact->inexact (average-length (partitions-4 '(60) '())))
;; 15.5

;; (map (λ (x) (partitions-4 (list x) '())) (range 1 5))
(void (map (λ (x) (display (format "~a," (exact->inexact x))))
           (map average-length
                (map (λ (x) (partitions-4 (list x) '())) (range 1 62 5)))))
;; 1.0,3.1818181818181817,4.910714285714286,6.333333333333333,7.641414141414141,8.834975369458128,9.958345513007892,11.014852311286644,12.024852522261849,12.989759184524148,13.919426697173913,14.816184942570844,15.685251514705685

#|
That doesn't seem so bad, let's say it goes to 25 max.
Numbers are overwhelming single digit so let's count numbers as one byte.

The data in ascii would have 25 * 190569291 numbers + 190569291 * 24 delims + 190569291 newlines
(* 50 190569291) 
= 9 528 464 550, about 10 gigabytes, and pointers are not bytes. Well.
*** now we see why we couldn't generate them in memory ***
|#
