#lang racket/base

; knight moves look like this
; xxxxxxx (-1, 2) xxxxxxx ( 1, 2) xxxxxxx
; (-2, 1) xxxxxxx xxxxxxx xxxxxxx ( 2, 1)
; xxxxxxx xxxxxxx kkkkkkk xxxxxxx xxxxxxx
; (-2,-1) xxxxxxx xxxxxxx xxxxxxx ( 2,-1)
; xxxxxxx (-1,-2) xxxxxxx ( 1,-2) xxxxxxx

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
                    [xxx
                     (lambda (sign number)
                       (display sign) (display number)
                       (cond
                         ((eq? sign '+) number)
                         ((eq? sign '-) (- 0 number))
                         (else 'ERROR)))]
                     [inner
                      (lambda (plus-minus one-two)
                        (cond
                          ((null? plus-minus) '())
                          ; (... (+ -) (1 2) -> (1 -2)
                          (else (cons
                                 (list (xxx (car (car plus-minus)) (car one-two)) (xxx (car (cdr (car plus-minus))) (car (cdr plus-minus))))
                                 (inner (cdr plus-minus) one-two)))))])
                     (append
                      (inner plus-minus (car one-two))
                      (inner plus-minus (car (cdr one-two))))))]
                          
             ); endletrec definitions
      (combine (multipermutations '(+ -) 2 0) (filter (lambda (x) (eq? (length x) 2)) (permutations '(1 2) 0 2)))
      )))



; we're backtrack searching for a tree that goes say (side * side) deep so that we moved to every part of the board
(define knight-tour
  (lambda (board side-length current-x current-y depth)
    (print-board board) ; debug
    (cond
      ((= depth (* side-length side-length)) (print-board board)); we've got a winner
      )))


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

; this is highly redundant
(define check-move
  (lambda (board x y)
    (letrec
        ([for-y (lambda (row y)
                  (cond
                    ((= y 0) (= (car row) empty-square))
                    (else (for-y (cdr row) (sub1 y)))))]
         [for-x (lambda (board x)
                  (cond
                    ((= x 0) (for-y (car board) y))
                    (else (for-x (cdr board) (sub1 x)))))])
      (for-x board x))))

(define empty-board
  (lambda (side-length)
    (letrec
        ([inner
          (lambda (depth-to-go)
            (cond
              ((= depth-to-go 0) '())
              ((cons (empty-row side-length) (inner (sub1 depth-to-go))))))])
      (inner side-length))))

(define empty-square -1)            

(define empty-row
  (lambda (side-length)
    (cond
      ((= side-length 0) '())
      ((cons empty-square (empty-row (- side-length 1)))))))

;(knight-tour (empty-board 8) 8 0 0 0)
;(check-move (add-move (empty-board 3) 2 2 55) 2 1)
(knight-moves 0)