#lang racket/gui

;;;;;;;;;;;SYNOPSIS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor canvas provided with text editor provided with editor keymap
;; pasteboard canvas provided with pasteboard provided with pasteboard keymap
;; custom canvas functions are provided to the keymap
;; create menu bar
;; show frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; frames and canvases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new frame% [label "Snip Edit"]
                      [width 1000]
                      [height 800]))

; left in derived classes to allow flexibility later
; for now, I've left out the complexity of associating
; canvas dependent behavior (i.e. when a text% instance (buffer) is in the top
; window it behaves differently than when in bottom window

(define top-editor-canvas%
  (class editor-canvas%
    (super-new)))

(define bot-pasteboard-canvas%
  (class editor-canvas%
    (super-new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; create editors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define text (new text%))
(define pasteboard (new pasteboard%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup editor canvas with editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define editor-canvas
  (new top-editor-canvas%
       [parent frame]))

(send editor-canvas set-editor text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup pasteboard canvas with editor 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pasteboard-canvas
  (new bot-pasteboard-canvas%
       [parent frame]))

(send pasteboard-canvas set-editor pasteboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; keymap for pasteboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;text editor focus operations

(define (editor-paste)
  (send text paste))

(define (editor-copy)
  (send text copy))

(define (editor-select-line)
  (send text move-position 'left #f 'line)
  (send text move-position 'right #t 'line))

(define (editor-copy-to-other-editor)
  (editor-copy)
  (send pasteboard paste))

(define (editor-copy-line-other-editor)
  (editor-select-line)
  (editor-copy-to-other-editor))

(define (editor-copy-from-other-editor)
  (pasteboard-copy-to-other-editor))

(define (editor-cut-line)
    (editor-select-line)
    (send text cut))

(define (editor-zen-to-han-line)
  (editor-cut-line)
  (set-clipboard-contents (zenkaku-numbers-to-hankaku (get-clipboard-contents)))
  (editor-paste))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pasteboard editor focus operations

(define (pasteboard-copy-to-other-editor)
  (send pasteboard copy)
  (send text paste))

; create a keymap for both pasteboard and editor
(define pasteboard-keymap (new keymap%))
(define editor-keymap (new keymap%))

; add common functions
(add-text-keymap-functions pasteboard-keymap)
(add-text-keymap-functions editor-keymap)

;; add and map a function wrapped to take 2 variables and throw them away for now
(define (add-function-to-keymap keymap name func key)
  (send keymap add-function name (lambda (dummy1 dummy2) (func)))
  (send keymap map-function key name) )

;; map keys to functions
(add-function-to-keymap editor-keymap
                        "ectoe"
                        editor-copy-to-other-editor
                        "c:/")

(add-function-to-keymap editor-keymap
                         "ecltoe"
                         editor-copy-line-other-editor
                         "left")

(add-function-to-keymap editor-keymap
                        "ecfoe"
                        editor-copy-from-other-editor
                        "right")

(add-function-to-keymap pasteboard-keymap
                        "pbctoe"
                        pasteboard-copy-to-other-editor
                        "left")

(add-function-to-keymap editor-keymap
                        "ezthl"
                        editor-zen-to-han-line
                        "c:n")

; attach/give keymap to editor
(send text set-keymap editor-keymap)

; attach/give the keymap to the pasteboard
(send pasteboard set-keymap pasteboard-keymap)

; create a menu bar for funs
(define menu-bar (new menu-bar% [parent frame]))
(define menu-edit (new menu% [label "Edit"] [parent menu-bar]))
(define menu-font (new menu% [label "Font"] [parent menu-bar]))
(append-editor-operation-menu-items menu-edit #f)
(append-editor-font-menu-items menu-font)

; let user undo
(send text set-max-undo-history 100)

(send frame show #t)
