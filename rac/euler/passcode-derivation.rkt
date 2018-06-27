#lang racket



;; A common security method used for online banking is to ask the user for three random characters from a passcode. For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.
;; The text file, keylog.txt, contains fifty successful login attempts.
;; Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.

(define keylog '(319 680 180 690 129 620 762 689 762 318 368 710 720 710 629 168 160 689 716 731 736 729 316 729 729 710 769 290 719 680 318 389 162 289 162 718 729 319 790 680 890 362 319 760 316 729 380 319 728 716))

; so we have something like
;   31                9
;      6           8       0
;    1             8       0
;      6              9    0
;    1           2    9
;      6         2         0
; 7    6         2
;      6           8  9
; 7    6         2
;   31             8
;   3  6           8
; 7  1                     0
; 7              2         0
;      6         2    9
;    1 6           8
;    1 6                   0
;      6           8  9
; 7  1 6
; 7 31
; 7 3  6
; 7              2    9
;   31 6
; 7    6              9
;                2    9    0
; 7  1                9
;    1 6         2
;                2 8  9

; 7  1             8
;   31                9
; 7                   9    0
;                  8  9    0
;   3  6         2

; so the answer is
; (redacted)
