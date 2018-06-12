#lang racket/gui

(require racket/gui/base)

; main "frame"
(define frame (new frame% [label "Snip Edit"]
                      [width 800]
                      [height 500]))

(define msg (new message% [parent frame]
                          [label "No events so far..."]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; use inheritance to override base class key handlers for canvases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define top-editor-canvas%
  (class
    editor-canvas% ; base class
    ; override method handling keyboard events
    (define/override (on-char event)

      ; debug line to examine keys
      ;(eprintf "got ~v\n" (send event get-key-code))
      
      )
    ; Call the superclass init, passing on all init args
    (super-new)))

(define bot-pasteboard-canvas%
  (class
    editor-canvas% ; base class
    ; override method handling keyboard events
    (define/override (on-char event)

      ; debug line to examine keys
      ;(eprintf "got ~v\n" (send event get-key-code))

      )
    ; Call the superclass init, passing on all init args
    (super-new)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup canvas with editor/pasteboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define editor-canvas
  (new top-editor-canvas%
       [parent frame]))

(define pasteboard-canvas
  (new bot-editor-canvas%
       [parent frame]))


(define text (new text%))

(define pasteboard (new pasteboard%))
(send editor-canvas set-editor text)
(send paste-canvas set-editor pasteboard)

(define menu-bar (new menu-bar% [parent frame]))
(define menu-edit (new menu% [label "Edit"] [parent menu-bar]))
(define menu-font (new menu% [label "Font"] [parent menu-bar]))


(append-editor-operation-menu-items menu-edit #f)
(append-editor-font-menu-items menu-font)
(send text set-max-undo-history 100)

(send frame show #t)



