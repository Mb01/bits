#!/usr/bin/env racket
#lang racket/base


(define side-length 8)
(define empty-square 0)
(define initial-depth 0)
(define count 0) ; debug counter

; knight moves look like this
; xxxxxxx (-1, 2) xxxxxxx ( 1, 2) xxxxxxx
; (-2, 1) xxxxxxx xxxxxxx xxxxxxx ( 2, 1)
; xxxxxxx xxxxxxx kkkkkkk xxxxxxx xxxxxxx
; (-2,-1) xxxxxxx xxxxxxx xxxxxxx ( 2,-1)
; xxxxxxx (-1,-2) xxxxxxx ( 1,-2) xxxxxxx


; note that complete tours are obligated to move to the corner when it is a legal move, otherwise it is not possible to enter and exit the corner


; one needlessly complicated way to think about these moves: permutations of (1, 2) combined with multipermutations of (+, -)
(define knight-moves ; the hard way
  (lambda (x)
    (letrec (
             [add-to-each; cons an (el)ement to the front of each list in a (li)st
              (lambda (el li)
                (cond
                  ((null? li) '())
                  (else (cons (cons el (car li)) (add-to-each el (cdr li))))))]
             [rotate-left (lambda (li) (letrec ([inner (lambda (li carli) (cond ((null? li) (list carli)) (else (cons (car li) (inner (cdr li) carli)))))])(inner (cdr li) (car li))))]
             [n-empty-lists
              (lambda (n)
                (cond
                  ((eq? n 0) '())
                  (else (cons '() (n-empty-lists (sub1 n))))))]
             [permutations
              (lambda (li depth original-length)
                ; if we are as deep as the original length (counting from 0), then we have considered all elements at start
                ; therefore, we are finished
                (cond
                  ((= depth original-length) '(()))
                  ; an element can either be at the start of a permutation or not
                  (else (append 
                         ; case in which at start, then we find all the permutations without that element
                         (add-to-each (car li) (permutations (cdr li) 0 (sub1 original-length)))
                         ; or not, we will rotate the list and set a different first element
                         (permutations (rotate-left li) (add1 depth) original-length)))))]
             [multipermutations
              (lambda (li n depth)
                (cond
                  ((= 0 n) '(()))
                  ((= depth (length li)) '())
                  ; as in permutations, an element either starts or does not start the list, however it can be in multiple positions
                  (else (append
                         ; case in which at start, then we find all the permutationos WITH that element remaining
                         ; our search depth is reset to 0 but our n is subtracted by one
                         ; if we reset the ordering of li here, then we can preserve the lexicographic ordering but that is not needed
                         (add-to-each (car li) (multipermutations li (sub1 n) 0))
                         ; case in which not at start, the number of remaining elements is the same but we have increased our search depth
                         (multipermutations (rotate-left li) n (add1 depth))))))]
             [combine; or "map" for idiots I guess
              (lambda (plus-minus one-two)
                (letrec (
                         [interpret-sign
                          (lambda (sign number)
                            (cond
                              ((eq? sign '+) number)
                              ((eq? sign '-) (- 0 number))
                              (else (error "we need '+ or '-"))))]
                         [inner
                          (lambda (plus-minus one-two)
                            (cond
                              ((null? plus-minus) '())
                              ; (... (+ -)) (... (1 2)) -> (... (1 -2))
                              (else (cons
                                     (list (interpret-sign (car (car plus-minus)) (car one-two)) (interpret-sign (car (cdr (car plus-minus))) (car (cdr one-two))))
                                     (inner (cdr plus-minus) one-two)))))])
                  (append
                   (inner plus-minus (car one-two))
                   (inner plus-minus (car (cdr one-two))))))]
             
             ); endletrec definitions
      (combine (multipermutations '(+ -) 2 0) (filter (lambda (x) (eq? (length x) 2)) (permutations '(1 2) 0 2)))
      )))

(define all-moves (knight-moves initial-depth))


;;;;; main function ;;;;;
; we're backtrack searching for a tree that goes say (side * side) deep so that we moved to every part of the board

(define knight-tour
  (lambda (board side-length current-x current-y moves depth)
    ;(print-board board); debug
    ;(set! count (+ count 1))
    ;(display count) (display " : ") (display depth) (newline)
    (let ([updated-x (if (null? moves) -1 (+ current-x (car (car moves))))]; put it out of range if out of moves
          [updated-y (if (null? moves) -1 (+ current-y (car (cdr (car moves)))))])
      (cond
        ((null? moves) #f); out of moves, go back
        ((= depth (* side-length side-length)) (print-board board) #f); we've got a winner, reached recursion limit, go back
        
        ; check if our current position is valid, then we can keep going
        ((check-move board current-x current-y side-length)
         (knight-tour (add-move board current-x current-y depth)
                      side-length
                      updated-x
                      updated-y
                      all-moves; our moves are all available again
                      (add1 depth))
         ; try the rest of our moves
         (knight-tour board
                      side-length
                      current-x
                      current-y
                      (cdr moves) ; we used up the last move
                      depth  ; the depth is the same
                      ))))))

(define print-board
  (lambda (board)
    (cond
      ((null? board) (newline))
      (else (display (car board)) (newline) (print-board (cdr board))))))

; add a move
(define add-move
  (lambda (board x y move-number)
    (letrec
        ([for-y (lambda (row y)
                  (cond
                    ((= y 0) (cons move-number (cdr row)))
                    (else (cons (car row) (for-y (cdr row) (sub1 y))))))]
         [for-x (lambda (board x)
                  (cond
                    ((= x 0) (cons (for-y (car board) y) (cdr board)))
                    (else (cons (car board) (for-x (cdr board) (sub1 x))))))])
      (for-x board x))))

; highly redundant way of checking whether a move is valid
(define check-move
  (lambda (board x y side-length)
    ;(print-board board) (display x) (display ":") (display y)
    (letrec
        ([for-y (lambda (row y)
                  (cond
                    ((= y 0) (= (car row) empty-square))
                    (else (for-y (cdr row) (sub1 y)))))]
         [for-x (lambda (board x)
                  (cond
                    ((= x 0) (for-y (car board) y))
                    (else (for-x (cdr board) (sub1 x)))))])
      (and (> x -1) (> y -1) (< x side-length) (< y side-length) (for-x board x)))))

(define empty-board
  (lambda (side-length)
    (letrec
        ([inner
          (lambda (depth-to-go)
            (cond
              ((= depth-to-go 0) '())
              ((cons (empty-row side-length) (inner (sub1 depth-to-go))))))])
      (inner side-length))))



(define empty-row
  (lambda (side-length)
    (cond
      ((= side-length 0) '())
      ((cons empty-square (empty-row (- side-length 1)))))))

;(knight-tour (empty-board 8) 8 0 0 0)
;(check-move (add-move (empty-board side-length) 2 1 55) 2 2 side-length); -> #f
;(check-move (add-move (empty-board side-length) 2 2 55) 2 2 side-length); -> #f
;(knight-moves initial-depth)

(knight-tour (empty-board side-length) side-length 0 0 (knight-moves initial-depth) (add1 initial-depth))
