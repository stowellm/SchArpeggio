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
#lang racket
(require graphics/turtles)
(require racket/include)
(include "objects.rkt")

; quick-and-easy way to save each sheet
(define (save-music-sheet filename)
  (save-turtle-bitmap (string-append "/Users/mike/Desktop/" filename ".png") 'png)
)

; variable that remembers how many staves have been created
(define num-staves 0)

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


; draw the notes from the progression passed in by the driver
(define (draw-progression progression)
  (if (null? progression)
      'progression-drawn
      (begin
        (draw-chord (car progression))
        (draw-progression (cdr progression))
      )
  )
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

; draws a chord
(define (draw-chord chord)
  ; draw the root note
  (draw-note (chord 'root-note) (chord 'speed))
  
  ; TODO - draw the rest of the notes
)

(define (draw-note note speed)
  ; move the turtle in place
  (move-offset 30 (note 'height))
  ; draw the circle
  (make-note-circle 36)
  ; draw the appropriate note flag
  (draw-flag (note 'flag-pos-up?) speed)
  (draw-extra-staff-line (note 'name))
  (draw-fill-in speed)
  (draw-sharp (note 'sharp?))
  (move-offset 0 (- 0 (note 'height)))
)

; draws the flag if needed on the note
(define (draw-flag flag-up speed)
  ; if whole, no need to draw any flag
  (if (not (equal? speed whole))
    (if flag-up
        (begin
          (move-offset 5 -7)
          (draw-offset 0 -30)
          ; if eighth, we have one more line to draw
          (if (eq? speed eighth)
              (begin
                (draw-offset 10 0)
                (move-offset -15 37)
              )
              (move-offset -5 37)
          )
        )
        (begin
          (move-offset -6 -7)
          (draw-offset 0 30)
          ; if eighth, we have one more line to draw
          (if (eq? speed eighth)
              (begin
                (draw-offset -10 0)
                (move-offset 16 -23)
              )
              (move-offset 6 -23)
          )
        )
    )
    (do-nothing)
 )
)

; draws the extra staff line if needed for the note
(define (draw-extra-staff-line name)
  ; extra line through the note
  (cond ((or (equal? name 'c4)
          (equal? name 'c2)
          (equal? name 'e2)
          (equal? name 'c6)
          (equal? name 'a5))
        (begin
          (move-offset 0 -5)
          (draw-offset 10 0)
          (move-offset -10 0)
          (draw-offset -12 0)
          (move-offset 12 5)
        ))
      ; extra line below the note
      ((or (equal? name 'd4)
           (equal? name 'b3)
           (equal? name 'd2)
           (equal? name 'f2)
           (equal? name 'b5))
        (begin
          (draw-offset 10 0)
          (move-offset -10 0)
          (draw-offset -12 0)
          (move-offset 12 0)
        ))
      ; no extra line
      (else do-nothing)
  )
)
  
; fill in the note if needed
(define (draw-fill-in speed)
  (if (or (equal? speed fourth)
          (equal? speed eighth))
      (begin
        'TODO
      )
      (do-nothing)
  )
)

; draw the sharp on the note if needed
(define (draw-sharp is-sharp)
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

; test function
(define (test)
  (turtles #t)
  (draw-new-staff)
  (define chord (make-chord "e" "a" "a" "a" "a"))
  (draw-chord chord)
  (draw-note (hash-ref note-with-name 'c4) 'eighth)
  (draw-note (hash-ref note-with-name 'c2) 'quarter)
  (draw-note (hash-ref note-with-name 'd2) 'eighth)
  (draw-note (hash-ref note-with-name 'e2) 'quarter)
  (draw-note (hash-ref note-with-name 'f2) 'eighth)
  (draw-note (hash-ref note-with-name 'g2) 'quarter)
  (draw-note (hash-ref note-with-name 'c6) 'eighth)
  (draw-note (hash-ref note-with-name 'b5) 'quarter)
  (draw-note (hash-ref note-with-name 'a5) 'eighth)
  (draw-note (hash-ref note-with-name 'g5) 'quarter)
  (draw-note (hash-ref note-with-name 'f5) 'eighth)
)

