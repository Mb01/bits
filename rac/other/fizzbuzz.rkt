#lang racket

;; obligatory?

(define (fizz-buzz start stop)
  (unless (> start stop) '()
          (let ([three (zero? (modulo start 3))]
                [five (zero? (modulo start 5))])
            (cond
              ((or three five)
               (when three
                 (display "fizz"))
               (when five
                 (display "buzz")))
              (else (display start)))
              (newline)
              (fizz-buzz (add1 start) stop))))

;(fizz-buzz 0 100)

; more functional way

(define (fizz-buzz2 n)
  (let ([three (zero? (modulo n 3))] [five (zero? (modulo n 5))])
    (if (or three five)
        (string-append (if three "fizz" "") (if five "buzz" ""))
        n)))

(display (map fizz-buzz2 (range 100)))
    
