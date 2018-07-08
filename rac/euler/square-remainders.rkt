 #lang racket

;; Let r be the remainder when (a−1)^n + (a+1)^n is divided by a^2.

;; For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728 ≡ 42 mod 49. (a^2)

;; And as n varies, so too will r, but for a = 7 it turns out that rmax = 42.

;; For 3 ≤ a ≤ 1000, find ∑ rmax.

(require "utils.rkt")

(define (r a n)
  (modulo
   (+ (expt (- a 1) n)
      (expt (+ a 1) n))
   (* a a)))

;; (r 7 3) -> 42

(define (for-a a)
  (define data (map (curry r a) (range (* a 2))))
  (apply max data))

(apply + (map for-a (range 3 1001)))

;; thought process

;; I used
;; (map (λ (x) (map (curry r x) (range 30))) (range 1 10)) ->

;; '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;   (2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0 2 0)
;;   (2 6 2 0 2 3 2 6 2 0 2 3 2 6 2 0 2 3 2 6 2 0 2 3 2 6 2 0 2 3)
;;   (2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8 2 8)
;;   (2 10 2 5 2 0 2 20 2 15 2 10 2 5 2 0 2 20 2 15 2 10 2 5 2 0 2 20 2 15)
;;   (2 12 2 0 2 24 2 12 2 0 2 24 2 12 2 0 2 24 2 12 2 0 2 24 2 12 2 0 2 24)
;;   (2 14 2 42 2 21 2 0 2 28 2 7 2 35 2 14 2 42 2 21 2 0 2 28 2 7 2 35 2 14)
;;   (2 16 2 48 2 16 2 48 2 16 2 48 2 16 2 48 2 16 2 48 2 16 2 48 2 16 2 48 2 16)
;;   (2 18 2 54 2 9 2 45 2 0 2 36 2 72 2 27 2 63 2 18 2 54 2 9 2 45 2 0 2 36))

;; to see behavior and
;(map (curry r 9) (range 50))
;; to empirically see if repeats are forever (looks like yes)

;; and I was able to find the length of the cycles using

;; one way is to brute force, the other is put an upper bound on the cycle length
(define (cycle li) ;; if we know there is a cycle otherwise never returns
  (define (iter n)
    (if (equal? (take li n) (take (drop li n) n)) n
        (iter (add1 n))))
  (iter 1))

;; (cycle 1 '(1 2 3 4 1 2 3 4)) -> 4

;; (define for-nine (map (curry r 9) (range 50)))
;; (take for-nine (cycle for-nine)); -> '(2 18 2 54 2 9 2 45 2 0 2 36 2 72 2 27 2 63)

#;(define (cycle-n n)
  (displayln n)
  (define data (map (curry r n) (range (* 4 n))))
  (take data (cycle data))); -> '(2 18 2 54 2 9 2 45 2 0 2 36 2 72 2 27 2 63)

;;(cycle-n 9)

;;(map length (map cycle-n (range 1 100)))

;; so it looks like we can safely cap cycle length at (* a 2)


