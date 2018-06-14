#lang racket/gui

;;;;;;;;;;;SYNOPSIS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor canvas provided with text editor provided with editor keymap
;; pasteboard canvas provided with pasteboard provided with pasteboard keymap
;; custom canvas functions are provided to the keymap
;; create menu bar
;; show frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parent frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new frame% [label "Snip Edit"]
                      [width 800]
                      [height 500]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use inheritance to create custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define top-editor-canvas%
  (class editor-canvas%
    (init o-editor)
    (super-new)
    (define other-editor o-editor)

    (define/public (copy-to-other-editor)
      (let ([this-editor (send this get-editor)])
        (send this-editor copy)
        (send other-editor paste)
        ))))

(define bot-pasteboard-canvas%
  (class editor-canvas%
    (init o-editor)
    (super-new)
    (define other-editor o-editor)
    (define/public (copy-to-other-editor)
      (let ([this-editor (send this get-editor)])
        (send this-editor copy)
        (send other-editor paste)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; create editors for the two canvases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define text (new text%))
(define pasteboard (new pasteboard%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup editor canvas with editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define editor-canvas
  (new top-editor-canvas%
       [parent frame]
       [o-editor pasteboard]
       ))

(send editor-canvas set-editor text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup pasteboard canvas with editor 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pasteboard-canvas
  (new bot-pasteboard-canvas%
       [parent frame]
       [o-editor text]))

(send pasteboard-canvas set-editor pasteboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; keymap for pasteboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; expose editor copy-to-other-editor
(define (editor-copy-to-other-editor)
  (send editor-canvas copy-to-other-editor))

; expose pasteboard copy-to-other-editor
(define (pasteboard-copy-to-other-editor)
  (send pasteboard-canvas copy-to-other-editor))

; create a keymap for both pasteboard and editor
(define pasteboard-keymap (new keymap%))
(define editor-keymap (new keymap%))

; add common functions
(add-text-keymap-functions pasteboard-keymap)
(add-text-keymap-functions editor-keymap)

;; add and map a function wrapped to take 2 variables and throw them away for now
(define (add-function-to-keymap keymap name func key)
  (send keymap add-function name (lambda (dummy1 dummy2) (func)))
  (send keymap  map-function key name) )

;; map two keys to the function
(add-function-to-keymap pasteboard-keymap "pbctoe" pasteboard-copy-to-other-editor "v")
(add-function-to-keymap editor-keymap "ectoe" editor-copy-to-other-editor "c")


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
