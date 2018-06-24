#lang racket

(require racket/stream racket/generator)


;; question from https://www.geeksforgeeks.org/find-n-th-node-in-preorder-traversal-of-a-binary-tree/
;; Given a Binary tree and a number N, write a program to find the N-th node in the Preorder traversal of the given Binary tree.

;; https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/

;;       1
;;      / \
;;     /   \
;;    /     \
;;   2       3
;;  / \   
;; 4   5  

;; Depth First Traversals:
;; (a) Inorder (Left, Root, Right) : 4 2 5 1 3
;; (b) Preorder (Root, Left, Right) : 1 2 4 5 3
;; (c) Postorder (Left, Right, Root) : 4 5 2 3 1

(define (counter n)
  (define (count start step)
    (in-range start +inf.0 step))
  (generator ()
             (define c (count n 1))
             (let loop ([i c])
               (yield (stream-first i))
               (loop (stream-rest i)))))

(define count (counter 1))

(define (answer-question tree n)
  (call/cc ; want to stop recursing over a structure, call/cc
   (λ (return)
     (letrec ([recursive
               (λ (tree)
                 (if (or (not tree) (null? tree)) #f
                     (let ([count (count)])
                       (cond
                         ((= n count) (return (car tree)))
                         (else ; answer for left child then the right child
                               (recursive (cadr tree))
                               (recursive (caddr tree)))))))])
       (recursive tree)))))

(define (reset-counter!)
  (set! count (counter 1)))

;;  Input:      N = 4
;;              11
;;            /   \
;;           21    31
;;         /   \
;;        41     51

(define tree1 '(11 (21 (41 #f #f) (51 #f #f)) (31 #f #f)))
(define n1 4)
(answer-question tree1 n1)

;; Input:      N = 5
;;             25
;;           /    \
;;         20    30
;;        /    \ /   \
;;      18    22 24   32

(reset-counter!)
(define tree2 '(25 (20 (18 #f #f) (22 #f #f)) (30 (24 #f #f) (32 #f #f))))
(define n2 5)
(answer-question tree2 n2)
