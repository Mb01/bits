#lang racket

;examples from https://docs.racket-lang.org/guide/classes.html

(define fish% (class object%
  (init size)                ; initialization argument
 
  (define current-size size) ; field
 
  (super-new)                ; superclass initialization
 
  (define/public (get-size)
    current-size)
 
  (define/public (grow amt)
    (set! current-size (+ amt current-size)))
 
  (define/public (eat other-fish)
    (grow (send other-fish get-size)))))

#;(define hungry-fish% (class fish% (super-new)
                       (define/public (eat-more fish1 fish2)
                         (send this eat fish1)
                         (send this eat fish2))))

;(define charlie (new fish% [size 10]))

;(define size-10-fish% (class fish% (super-new [size 10])))

(define default-10-fish% (class fish%
                           (init [size 10])
                           (super-new [size size])))

 (new default-10-fish%)

