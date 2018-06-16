#lang racket ; I want range


;; prints a tree structure (without supplying any node data)


 ;;                               0
 ;;                              / \
 ;;                             /   \
 ;;                            /     \
 ;;                           /       \
 ;;                          /         \
 ;;                         /           \
 ;;                        /             \
 ;;                       /               \
 ;;                      /                 \
 ;;                     /                   \
 ;;                    /                     \
 ;;                   /                       \
 ;;                  /                         \
 ;;                 /                           \
 ;;                /                             \
 ;;               0                               0
 ;;              / \                             / \
 ;;             /   \                           /   \
 ;;            /     \                         /     \
 ;;           /       \                       /       \
 ;;          /         \                     /         \
 ;;         /           \                   /           \
 ;;        /             \                 /             \
 ;;       0               0               0               0
 ;;      / \             / \             / \             / \
 ;;     /   \           /   \           /   \           /   \
 ;;    /     \         /     \         /     \         /     \
 ;;   0       0       0       0       0       0       0       0
 ;;  / \     / \     / \     / \     / \     / \     / \     / \
 ;; 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0



;; actual tree I used has 3, 3 spacing at bottom
;;...............0
;;............../.\
;;............./...\
;;............/.....\
;;.........../.......\
;;........../.........\
;;........./..apart ...\
;;......../...apart.-2..\
;;+1.....0...............0
;;+1..../.\...spaced.+2./.\
;;+1.../.-2\..spaced.../...\
;;+1../..-2.\.spaced../.....\
;;+1.0.......0.......0--pair-0
;;../.\..+2./.\...../.\...../.\
;;.0...0.+20...0...0...0...0...0

;; so this looks like we write each time moved over once
;; 4 pairs of 3  apart 0s spaced 3
;; 4 pairs of 1  apart /s spaced 5

;; 2 pairs of 7  apart 0s spaced 7
;; 2 pairs of 5  apart /s spaced 9
;; 2 pairs of 3  apart /s spaced 11 

;; 1 pair  of ... ok I see what's going on, "spaced" gets bigger by two
;; when "apart" gets to one, we add line of nodes for next level
;; and set both "apart" and "spaced" to "spaced + 2"


;; this leads to a math sequence such as

;;   2^3 + 2^2 * 3 + (2^2 - 1) * 3 + 0 = 29
;; = 2^3 + 2^2 * 1 + (2^2 - 1) * 5 + 2

;; = 2^2 + 2^1 * 7 + (2^1 - 1) * 7 + 4
;; = 2^2 + 2^1 * 5 + (2^1 - 1) * 9 + 6
;; = 2^2 + 2^1 * 3 + (2^1 - 1) *11 + 8
;; = 2^2 + 2^1 * 1 + (2^1 - 1) *13 + 10

;; = 2^1 + 2^0 * 15 + (2^0 -1) *15 + 12
;; = 2^1 + 2^0 * 13 + (2^0 -1) *17 + 14
;; = 2^1 + 2^0 * 11 + (2^0 -1) *19 + 16
;; = 2^1 + 2^0 *  9 + (2^0 -1) *21 + 18
;; = 2^1 + 2^0 *  7 + (2^0 -1) *23 + 20
;; = 2^1 + 2^0 *  5 + (2^0 -1) *25 + 22
;; = 2^1 + 2^0 *  3 + (2^0 -1) *27 + 24
;; = 2^1 + 2^0 *  1 + (2^0 -1) *29 + 26 ; sweet, the non-existent gap continues up to 29

;; = 2^0 + 2^-1 * 29 +(2^-1 - 1) * 29 + 28 =which we can easily see= 1 + 29/2 - 29/2 + 28 = 29

;; 

(define (pairs left right n apart spaced)
  (letrec
      ([space #\space]
       [spac (lambda (sp)
               (cond
                 ((= 1 n) '())
                 ((= 0 sp) (pairs left right (sub1 n) apart spaced))
                 (else (cons space (spac (sub1 sp))))))]
       [apar  (lambda (ap)
                (if (= 0 ap)
                    (cons right (spac spaced))
                    (cons space (apar (sub1 ap)))))])
    (cons left (apar apart))))

(define (make-tree level bottom-spacing)
  (letrec (
           [left #\/] [right #\\] [node #\0]
           [connector-levels
            (lambda (level apart spaced)
              (let ([apart-2 (- apart 2)]
                    [spaced+2 (+ spaced 2)])
            (if (> apart-2 0)
                (cons  (pairs left right (expt 2 level) apart-2 spaced+2)
                       (connector-levels level apart-2 spaced+2))
                (node-level (sub1 level) spaced+2 spaced+2))))]
           [node-level
            (lambda (level apart spaced) 
              (if (>= level 0)
                  (cons (pairs node node (expt 2 level) apart spaced)
                        (connector-levels level apart spaced))
                  (list (list node))))])
           (node-level (- level 2) bottom-spacing bottom-spacing)))

; format and print tree
(define (repeat n)
  (cond
    ((= n 0) '())
    (else (cons #\space (repeat (sub1 n))))))

(let ([tree (make-tree 5 3)])
  (for
      ([i (reverse tree)]
       [j (range 0 (length tree))])
    (displayln (list->string (append (repeat (- (length tree) j)) i)))))
