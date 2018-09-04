#lang racket

(define (gcd m n)
  (let ([r (remainder m n)])
    (cond
      [(zero? r) n]
      [else (gcd n r)])))

(define (gcd-trace m n)
  (cons n
        (let ([r (remainder m n)])
          (cond
            [(zero? r) '()]
            [else (gcd-trace n r)]))))

