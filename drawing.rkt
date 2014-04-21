;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SchArpeggio                                          ;;;
;;; Drawing Program                                      ;;;
;;; Mike Stowell                                         ;;;
;;;                                                      ;;;
;;; Class that contains the Turtle library               ;;;
;;; implementation to draw the musical staves and        ;;;
;;; other notes to complement the remainder of the       ;;;
;;; arpeggiator.                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; include statements and lang
;#lang racket
;(require graphics/turtles)
;(require racket/include)
;(include "objects.rkt")

; quick-and-easy way to save each sheet
(define (save-music-sheet filename)
  (save-turtle-bitmap (string-append "/Users/mike/Desktop/" filename ".png") 'png)
)

; variable that remembers how many staves have been created
(define num-staves 0)

; draw the notes from the progression passed in by the driver
(define (draw-progression progression)
  (if (null? progression)
      'progression-drawn
      (begin
        (draw-note ((car progression) 'root-note))
        (draw-progression (cdr progression))
      )
  )
)

; basic method to draw a new music staff
(define (draw-new-staff)
  (next-staff-start #t)
  (split* 
          ; treble clef
          (move-offset 0 20)  (move-offset 0 30) (move-offset 0 40)
          (move-offset 0 50)  (move-offset 0 60)
          ; bass clef
          (move-offset 0 80)  (move-offset 0 90) (move-offset 0 100)
          (move-offset 0 110) (move-offset 0 120))
  (draw 740)
  (next-staff-start #f)
)

; go to the next staff starting location
; boolean indicates whether or not to increment num-staves
(define (next-staff-start bool)
  (home)
  (move-offset -375 (+ -375 (* num-staves 160)))
  (if (equal? bool #t)
      (set! num-staves (+ num-staves 1))
      (move-offset 0 -160))
)

; draws a musical note
(define (draw-note note)
  (move-offset 30 0)
  (move-offset 0 (note 'height))
  (make-note-circle 36)
  (draw-flag note)
  (draw-extra-staff-line note)
  (draw-fill-in note)
  (draw-sharp note)
  (move-offset 0 (- 0 (note 'height)))
)

; draws the flag if needed on the note
(define (draw-flag note)
  'TODO
)

; draws the extra staff line if needed for the note
(define (draw-extra-staff-line note)
  'TODO     
)

; fill in the note if needed
(define (draw-fill-in note)
  'TODO
)

; draw the sharp on the note if needed
(define (draw-sharp note)
  'TODO
)

; helper function that does the actual drawing
(define (make-note-circle remaining)
  (turn 10)
  (draw 1)
  (if (= (- remaining 1) 0)
      (do-nothing)
      (make-note-circle (- remaining 1)))
)

; ease-of-use command to do nothing
(define (do-nothing)
  (turn 0)
)

