#lang racket

(require racket/generator)

;; for reference, see
;; Donald E. Knuth
;; "The Art of Computer Programming"
;; vol. 2

;; x-sub-n -> x-sub-n+1
(define (linear-congruential x a c m)
  (modulo (+ (* x a) c) m))

(define (lin-cong-generator x a c m)
  (generator ()
             (define (inner x)
               (let ([next (linear-congruential x a c m)])
                 (yield x)
                 (inner next)))
             (inner x)))

;; detects if we ever get back to the same number
;; (This only works if returning to the same number is a cycle.)
;; (Note the cycles 2 3 3 3 .... and 2 1 3 5 7 9 1 3 5 7 9 ...
;; don't return to 2.]
(define (find-cycle gen)
  (let ([init (gen)])
    (define (inner count)
      (cond
        [(> count 100000) #f] ; let's give up sometime soon
        [(= (gen) init) `(,init count ,count)]
        [else (inner (add1 count))]))
    (inner 1)))

;; detects if we ever get to a cycle
(define (find-cycle2 gen)
  (define (inner count seen)
    (let ([next (gen)])
      (cond
        [(set-member? seen next) count]
        [else (inner (add1 count) (set-add seen next))])))
  (inner 0 (set)))


(define first-test-generator (lin-cong-generator 7 7 7 10))
(define second-test-generator (lin-cong-generator 7 7 7 64))
(define third-test-generator (lin-cong-generator 7 7 7 128))

(find-cycle2 first-test-generator)
(find-cycle2 second-test-generator)
(find-cycle2 third-test-generator)


(map find-cycle2 (map (curry lin-cong-generator 7 7 7) (range 11 101)))
