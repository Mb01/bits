#lang racket

;; All square roots are periodic when written as continued fractions and can be written in the form:

;; see problem description

;; Exactly four continued fractions, for N ≤ 13, have an odd period.
;; How many continued fractions for N ≤ 10000 have an odd period?

(define (squared x)
  (* x x))

;; let's walk through the example
;; sqrt(23) = (sq23 + 0) / 1 =

;; --steps--

;; ** while positive "draw out" ones e.g. 5/4 = 1 + 1/4**
;; 4 + (sq23 - 4) / 1  = 4 + ...
;; ** then "continued-fractionify" **
;; 1 / (1 / (sq23 - 4)) 
;; state is kept values and 1 / (sq23 - 4)
;; ** rationalize denominator by multiplying conjugate **
;; 1 / (sq23 - 4) * (sq23 + 4) / (sq23 + 4) => (sq23 + 4) / 7
;; ** repeat **

;; --- let's break down that last step ---
;; a / (sqrt(b) + c) =
;; a (sqrt(b) - c) / (b - c^2)


;; insight, keep the multiplier "out front"

;; let's try to write that out and
;; try to skip intermediate steps
;; with unsolved algebra to manipulate

;; how many ones can we remove from the fraction
;; while the fraction is still positive
(define (factor-out-ones root-of addend denominator)
  (cond
    [(> (squared addend) root-of) -1]
    [else (add1 (factor-out-ones root-of (- addend denominator) denominator))]))

;(invert-rationalize-prototype2 23 -4 1)
;(invert-rationalize-prototype2 23 -3 7)

(define (solve root-of)
  ;; cycle-set contains previous arguments
  (define cycle-set (mutable-set))
  ;; state in the form:  ;; (sq(root-of) + add) / den
  ;; acc has "left-side addends" already found
  (define (solve add den acc)
    (cond
      ;; already seen = cycle -> finished
      [(set-member? cycle-set (list add den)) (reverse acc)]
      [else
       (set-add! cycle-set (list add den))
       (let* (;; factor out ones
              [ones (factor-out-ones root-of add den)]
              ;; update add to reflect this
              [add (- add ( * ones den))]
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

;; remember we have that first number that is not part of the period
(length (filter-map (lambda (x) (even? (length x))) raw-answer))
