#lang racket

(define n (* 2 3 5 7)); 210
(define d (* 2 11 13)) ; 286
(/ n d) ; 105 / 143

;; euclids algorithm works like this
(define (gcd n d)
  (displayln `(,n ,d))
  (let ([remainder (remainder n d)])
  (cond
    [(zero? remainder) d]
    [else (gcd d remainder)])))

(gcd n d)
(newline)

;; a continued fraction works like this

(define (cont n d)
  (displayln `(,n ,d))
  (let ([quotient (quotient n d)]
        [remainder (remainder n d)])
    (cond
      [(zero? remainder) d]
      [else (cont remainder (- d remainder))])))
    
(cont n d)

;; (This is a more traditional form)
(define (cont2 n d)
  (let ([quotient (quotient n d)]
        [remainder (remainder n d)])
    (cond
      [(zero? remainder) (list quotient)]
      [else (cons quotient (cont2 d remainder))])))
    
(cont2 n d)
