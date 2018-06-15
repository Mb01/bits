#lang racket/gui

;; some utility functions for user interface

(require racket/generator)

(provide get-clipboard-contents)
(provide set-clipboard-contents)
(provide zenkaku-numbers-to-hankaku)


;; we're throwing away our chance to use the timestamp of an event
;; hopefully the clipboard doesn't change from keypress to actual retrieval
(define (get-clipboard-contents)
  (send the-clipboard get-clipboard-string (current-milliseconds)))

(define (set-clipboard-contents a-string)
  (send the-clipboard set-clipboard-string a-string (current-milliseconds)))

; this might be useful for cycling through different snips
(define (make-ring-generator li)
  (generator ()
    (sequence-for-each
     (lambda (x) (yield x)) (in-cycle li))))

; replace full width numbers with appropriate half-width numbers
(define (zenkaku-numbers-to-hankaku a-string)
  (define (zenkaku-map a-char)
    (define zenkaku-hash (hash #\０ #\0  #\１ #\1   #\２ #\2   #\３ #\3   #\４ #\4   #\５ #\5 #\６ #\6 #\７ #\7 #\８ #\8 #\９ #\9))
    (if (hash-has-key? zenkaku-hash a-char)
        (hash-ref zenkaku-hash a-char)
        a-char))
  (list->string (map zenkaku-map (string->list a-string))))



