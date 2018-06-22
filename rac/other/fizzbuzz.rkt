#lang racket

;; obligatory?
;; first attempt

(define (fizz-buzz start stop)
  (unless (> start stop)
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

;(display (map fizz-buzz2 (range 100)))
    
;matching

(define (fizz-buzz3 n)
  (let ([three (modulo n 3)] [five (modulo n 5)])
    (match (list three five)
      [(list 0 0) "fizzbuzz"]
      [(list 0 x) "fizz"]
      [(list x 0) "buzz"]
      [x (number->string n)])))

;(display (string-join (map fizz-buzz3 (range 101)) "\n"))

;; how I would do this in C

(define (fizz-buzz4 start stop); stop inclusive
  (unless (> start stop)
    (let ([three (zero? (modulo start 3))] [five (zero? (modulo start 50))])
      (when three (display "fizz"))
      (when five (display "buzz"))
      (when (not (or three five)) (display start))
      (newline)
      (fizz-buzz4 (add1 start) stop))))

(fizz-buzz4 0 100)
