#lang racket/gui

(require framework)

; a text pasting pallete tool
; this is merely a toy

; synopsis
; inherit from editor-canvas% 
; to overide methods for custom callbacks (define a class)
; instantiate it
; instantiate text object / paster object
; assign it to respective canvases
; also set up a menu

(require racket/gui/base)

; parent frame
(define frame (new frame% [label "Snip Edit"]
                      [width 800]
                      [height 500]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use inheritance to override base class key handlers for canvases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define top-editor-canvas%
  (class editor-canvas%
    (super-new)
    ; override method handling keyboard events
    #;(define/override (on-char event)
      ; debug line to examine keys
      (eprintf "got ~v\n" (send event get-shift-down))
        
      )
    ; Call the superclass init, passing on all init args
    ))

(define bot-pasteboard-canvas%
  (class editor-canvas%
    (init o-editor)
    (super-new)
    (define other-editor o-editor)
    (define/public (copy-to-other-editor)
      (let ([this-editor (send this get-editor)])
        (send this-editor copy)
        (send other-editor paste)
        (displayln "copy-to-other-editor")))
    
    #;(define/public (on-char event); override method handling keyboard events
      (let ([key-code (send event get-key-code)]
            [this-editor (send this get-editor)])
        (printf "~a\n" key-code); debug line to examine keys
        (send this-editor copy)
        (send other-editor paste)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup editor canvas with editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define editor-canvas
  (new top-editor-canvas%
       [parent frame]))

(define text (new text%))
(send editor-canvas set-editor text)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup pasteboard canvas with editor 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define pasteboard-canvas
  (new bot-pasteboard-canvas%
       [parent frame]
       [o-editor text]
       ))


(define pasteboard (new pasteboard%))
(send pasteboard-canvas set-editor pasteboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; keymap for pasteboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pasteboard-keymap (new keymap%))

(add-text-keymap-functions pasteboard-keymap)

;;(send keymap map-function "c:x" "cut-clipboard")
;;(send keymap map-function "c:c" "copy-clipboard")
(send pasteboard-keymap map-function "v" "clipboard-copy")

;;(send keymap map-function "middlebutton" "paste-x-selection")
(send pasteboard set-keymap pasteboard-keymap)

(define menu-bar (new menu-bar% [parent frame]))
(define menu-edit (new menu% [label "Edit"] [parent menu-bar]))
(define menu-font (new menu% [label "Font"] [parent menu-bar]))


(append-editor-operation-menu-items menu-edit #f)
(append-editor-font-menu-items menu-font)
(send text set-max-undo-history 100)

(send frame show #t)



