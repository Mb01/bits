#lang racket

(require pict pict/tree-layout)
(require racket/generator racket/stream)

; reference
;(tree-layout [#:pict node-pict] child ...) → tree-layout?
;  node-pict : (or/c #f pict?) = #f
;  child : (or/c tree-layout? tree-edge? #f)

(define (counter n)
  (define (count start step)
    (in-range start +inf.0 step))
  (generator ()
             (define c (count n 1))
             (let loop ([i c])
               (yield (text (number->string (stream-first i)) "green" 14))
               (loop (stream-rest i)))))

(define number-pict (counter 0))

(define (complete depth)
  (cond
    ((zero? depth) #f)
    (else
     (letrec
         ([lchild (complete (- depth 1))]
          [rchild (complete (- depth 1))])
       (tree-layout #:pict (number-pict) lchild rchild)))))

(inset (binary-tidier (complete 6) #:x-spacing 9) 15)
