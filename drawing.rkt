#lang racket
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

; include statements
(require graphics/turtles)
(require racket/include)
(include "objects.rkt")

; turn on the drawing board
(turtles #t)

; quick-and-easy way to save each sheet
(define (save-music-sheet filename)
  (save-turtle-bitmap (string-append "/Users/mike/Desktop/" filename ".png") 'png)
)

; variable that remembers how many staves have been created
(define num-staves 0)

; all the notes we need
(define E 45)
(define F 40)
(define G 35)
(define A 30)
(define B 25)
(define C 20)
(define D 15)
(define e 10)
(define f 5)

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
  (move-offset 0 note)
  (note-helper 36)
  (move-offset 0 (- 0 note))
)

; helper function that does the actual drawing
(define (note-helper remaining)
  (turn 10)
  (draw 1)
  (if (= (- remaining 1) 0)
      (do-nothing)
      (note-helper (- remaining 1)))
)

; ease-of-use command to do nothing
(define (do-nothing)
  (turn 0)
)

