#lang racket

(require racket/stream pict/tree-layout pict racket/generator)

;(tree-layout [#:pict node-pict] child ...) → tree-layout?
;  node-pict : (or/c #f pict?) = #f
;  child : (or/c tree-layout? tree-edge? #f)

(define circ (disk 20 #:color "Chartreuse" #:border-color "Medium Aquamarine" #:border-width 5))

(define (counter n)
  (define (count start step)
    (in-range start +inf.0 step))
  (generator ()
             (define c (count n 1))
             (let loop ([i c])
               (yield (cc-superimpose circ (text (number->string (stream-first i)) "green" 17)))
               (loop (stream-rest i)))
               ))

(define number-pict (counter 0))

(define (reset-counter)
  (set! number-pict (counter 0)))

(define (complete depth)
    (cond
      ((zero? depth) #f)
      (else
       (let
           ([lchild (complete (- depth 1))]
            [rchild (complete (- depth 1))])
         (tree-layout #:pict (number-pict) lchild rchild)))))

(define (call-and-reset n)
  (reset-counter)
  (inset (binary-tidier (complete n) #:x-spacing 15) 20))

(define (call-and-reset2 n)
  (reset-counter)
  (inset (hv-alternating (complete n) #:x-spacing 30) 20))

(map (lambda (x) (call-and-reset x)) (range 4 8))
(map (lambda (x) (call-and-reset2 x)) (range 4 8))

