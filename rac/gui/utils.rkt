#lang racket/gui

;; some utility functions for user interface

(provide get-clipboard-contents)
(provide set-clipboard-contents)

;; we're throwing away our chance to use the timestamp of an event
;; hopefully the clipboard doesn't change from keypress to actual retrieval
(define (get-clipboard-contents)
  (send the-clipboard get-clipboard-string (current-milliseconds)))

(define (set-clipboard-contents a-string)
  (send the-clipboard set-clipboard-string a-string (current-milliseconds)))
