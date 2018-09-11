#lang racket

;; All square roots are periodic when written as continued fractions and can be written in the form:

;; see problem description

;; Exactly four continued fractions, for N ≤ 13, have an odd period.
;; How many continued fractions for N ≤ 10000 have an odd period?

(define (squared x)
  (* x x))

;; Forgive me for not reading up on the subject before attempting the problem.

;; let's walk through the example
;;     sqrt(23) = (sq23 + 0) / 1 =

;; ** While positive, move ones out of the fraction 0 +  5/4 -> 1 + 1/4 **
;;     4 + (sq23 - 4) / 1  = 

;; ** then "continued-fractionify" **
;;     4 + 1 / (1 / (sq23 - 4)) 
;;     (the 4 on the left is part of the answer)
;;     1 / (sq23 - 4)

;; ** rationalize the denominator by multiplying with conjugate **
;;     1 / (sq23 - 4) * (sq23 + 4) / (sq23 + 4) => (sq23 + 4) / 7

;; ** repeat **

;; --- this last step is  ---
;; that's:
;; a / (sqrt(b) + c) =
;; a (sqrt(b) - c) / (b - c^2)

;; ====== BRIEF EXPLANATION ======

;; I haven't proven it, but I guess this always simplifies to:

;; (sqrt(root-of) + add) / den -> (sqrt(root-of) - add') / den'

;; where
;; add' = add - ones * den
;; den' = (root-of - add'^2) / den
;; ones = factor-out-ones(root-of, add, den)

;; How many ones can we remove from the fraction
;; while the fraction is still positive?
;; todo: naming / proper implementation.
(define (factor-out-ones root-of addend denominator)
  (cond
    [(> (squared addend) root-of) -1]
    [else (add1 (factor-out-ones root-of (- addend denominator) denominator))]))

(define (solve root-of)
  ;; record arguments to detect cycle
  (define cycle-set (mutable-set))
  ;; state in the form:  ;; (sq(root-of) + add) / den
  ;; acc has "left-side addends" already found
  (define (solve add den acc)
    (cond
      ;; already seen arguments represents cycle -> finish
      [(set-member? cycle-set (list add den)) (reverse acc)]
      [else
       ;; record arguments
       (set-add! cycle-set (list add den))
       (let* (;; factor out ones
              [ones (factor-out-ones root-of add den)]
              ;; update add to reflect this
              [add (- add (* ones den))]
              ;; invert -> rationalize -> cancel multiplier
              [new-den (/ (- root-of (squared add)) den)]
              [new-add (- add)])
         ;; recurse
         (solve new-add new-den (cons ones acc)))]))
  (solve 0 1 '()))

;; (solve 23); -> '(4 1 3 1 8) note the first number is not part of the period

(define target-range (apply set (range 2 10001)))

(define squares (apply set (map squared (range 101))))

(define no-squares (set-subtract target-range squares))

(define raw-answer (set-map no-squares solve))

;; since the first number is not part of the answer, odd? -> even?
(length (filter-map (lambda (x) (even? (length x))) raw-answer))

;; the current definition style precludes solving for (1 + sqr(5))/ 2 etc. which is not satisfactory
