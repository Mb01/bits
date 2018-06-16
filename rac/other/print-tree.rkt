#lang racket ; I want range

;n=1
;0 1

;2
;..0.. 5
;./.\.
;0...0

;3
;.....0..... 11
;..../.\....
;.../...\...
;..0.....0..
;./.\.../.\.
;0...0.0...0


; actual tree
;...............0
;............../.\
;............./...\
;............/.....\
;.........../.......\
;........../.........\
;........./...........\
;......../.............\
;.......0...............0
;....../.\............./.\
;...../...\.........../...\
;..../.....\........./.....\
;...0.......0.......0.......0
;../.\...../.\...../.\...../.\
;.0...0...0...0...0...0...0...0




; so this looks like we write each time moved over once
; 4 pairs of 3  apart 0s spaced 3
; 4 pairs of 1  apart /s spaced 5

; 2 pairs of 7  apart 0s spaced 7
; 2 pairs of 5  apart /s spaced 9
; 2 pairs of 3  apart /s spaced 11 

; 1 pair  of 


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
            (if (> apart 0)
                (cons  (pairs left right (expt 2 level) apart spaced)
                       (connector-levels level (- apart 2) (+ spaced 2)))
                (node-level (sub1 level) spaced spaced)))]
           [node-level
            (lambda (level apart spaced)
              (if (>= level 0)
                  (cons (pairs node node (expt 2 level) apart spaced)
                        (connector-levels level (- apart 2) (+ spaced 2)))
                  (list (list node))))])
           (node-level (- level 2) bottom-spacing bottom-spacing)))


; format and print tree
(define (repeat n)
  (cond
    ((= n 0) '())
    (else (cons #\space (repeat (sub1 n))))))

(let ([tree (make-tree 4 3)])
  (for
      ([i (reverse tree)]
       [j (range 0 (length tree))])
    (displayln (list->string (append (repeat (- (length tree) j)) i)))))
