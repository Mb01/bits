#lang racket


; given a tree structure
; print
;(15)...........0
;(7)....1......(15).....2
;   3       4       5..(7)..6
; 7   8   9   a   b   c   d(3)e
;f g h i j k l m n 0 p q r s t u

(require racket/stream)
(require pict/tree-layout)
(require pict)
(require racket/generator)


;(tree-layout [#:pict node-pict] child ...) → tree-layout?
;  node-pict : (or/c #f pict?) = #f
;  child : (or/c tree-layout? tree-edge? #f)





(define (counter n)
  (define (count start step)
    (in-range start +inf.0 step))
  (generator ()
             (define c (count n 1))
             (let loop ([i c])
               (yield (text (number->string (stream-first i)) "green" 25))
               (loop (stream-rest i)))
               ))

(define number-pict (counter 0))

(define (complete depth)
    (cond
      ((zero? depth) #f)
      (else
       (letrec
           ([lchild (complete (- depth 1))]
            [rchild (complete (- depth 1))])            
         (tree-layout #:pict (number-pict) lchild rchild)))))

(binary-tidier (complete 7))

