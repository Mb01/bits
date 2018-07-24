#lang racket

(require "utils.rkt")



;; Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.

;; Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.

;; We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.

;; Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525) are bouncy. In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.

;; Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.

;; Find the least number for which the proportion of bouncy numbers is exactly 99%.

(define target 99/100)

;; well huge  swaths of numbers should be bouncy once we get big enough

;; for example 131000 will be bouncy until  133333

;; but a modern computer should be strong enough to brute force this I believe

;; inversion? twice is not optimal but pretty good
(define (bouncy? n)
  (let* ([li (integer->list n)]
         [il (reverse li)])
    (and (inversion? li) (inversion? il))))

(define (search n total-seen bouncy-seen)
  (let ([total-seen (add1 total-seen)]
        [bouncy-seen (+ bouncy-seen (if (bouncy? n) 1 0))])
    (cond
      [(>= (/ bouncy-seen total-seen) target) n]
      [else (search (add1 n) total-seen bouncy-seen)])))
