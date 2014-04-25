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
(define (draw-new-staff . is-right-staff?)
  (let ((draw-right (and (not (null? is-right-staff?)) is-right-staff?)))
    (if draw-right
      (next-right-staff-start #f)
      (next-left-staff-start #t))
    (split* 
            ; treble clef
            (move-offset 0 20)  (move-offset 0 30) (move-offset 0 40)
            (move-offset 0 50)  (move-offset 0 60)
            ; bass clef
            (move-offset 0 80)  (move-offset 0 90) (move-offset 0 100)
            (move-offset 0 110) (move-offset 0 120))
    (if draw-right
      (draw 215)
      (draw 500))
    (if draw-right
      (next-right-staff-start #t)
      (next-left-staff-start #f))
  )
  (draw-treble-clef)
  (draw-bass-clef)
)

; draw the treble clef on the staff
(define (draw-treble-clef)
  (move-offset 0 48)
  (draw-offset 5 -7)
  (draw-offset 5 0)
  (draw-offset 5 7)
  (draw-offset -5 7)
  (draw-offset -12 0)
  (draw-offset -4 -7)
  (draw-offset 22 -27)
  (draw-offset 0 -4)
  (draw-offset -5 -5)
  (draw-offset -5 5)
  (draw-offset 0 55)
  (draw-offset -7 -7)
  (draw-offset 5 0)
  (move-offset -4 -65)
)

; draw the bass clef on the staff
(define (draw-bass-clef)
  (move-offset 4 92)
  (draw-offset 0 5)
  (draw-offset -3 3)
  (draw-offset -4 0)
  (draw-offset -3 -3)
  (draw-offset 0 -8)
  (draw-offset 3 -4)
  (draw-offset 13 0)
  (draw-offset 3 4)
  (draw-offset 0 10)
  (draw-offset -5 7)
  (draw-offset -14 9)
  (move-offset 25 -25)
  (draw-offset 0 2)
  (move-offset 0 2)
  (draw-offset 0 2)
  (move-offset -18 -96)
)


; draw the notes from the progression passed in by the driver
; on a right-sided staff
(define (draw-progression progression)
  (draw-new-staff #t)
  (progression-helper progression)
  (draw-repeat)
)

(define (progression-helper progression)
  (if (null? progression)
      (nothing)
      (begin
        (draw-chord (car progression))
        (progression-helper (cdr progression))
      )
  )
)

; draw the repeat symbol on the sheet
(define (draw-repeat)
  (home)
  (move-offset 245 225)
  (draw-offset 30 0)
  (draw-offset 0 -30)
  (draw-offset -30 0)
  (draw-offset 0 15)
  (draw-offset 5 -5)
  (move-offset -5 5)
  (draw-offset -5 -5)
)

; get the turtle off the screen so we can enjoy the music sheet
; without an obstruction
(define (disappear)
  (move-offset 1000 1000)
)

; position the turtle in place to draw a right staff
; boolean indicates whether or not to position 10 more pixels
; over to begin drawing notes
(define (next-right-staff-start bool)
  (home)
  (move-offset 165 -375)
  (if bool
      (move-offset 10 0)
      (nothing))
)

; go to the next left staff starting location
; boolean indicates whether or not to increment num-staves
(define (next-left-staff-start bool)
  (home)
  (move-offset -375 (+ -375 (* num-staves 160)))
  (if bool
      (set! num-staves (+ num-staves 1))
      (move-offset 10 -160))
)

; draws a chord
(define (draw-chord chord)
  ; draw the root note with a whole speed - chords are all wholes
  (draw-note (chord 'root-note) whole)
  ; draw any other notes in the chord
  (draw-chord-rest (chord 'notes))
)

(define (draw-chord-rest rest)
  (if (eq? rest '())
      (nothing)
      (begin
        (draw-note (car rest) whole)
        (move-offset -30 0)
        (draw-chord-rest (cdr rest))
      )
  )
)

; method to be called either by the draw-chord procedure
; internally or by Jeremy's arpegiator library to draw
; notes on the staves
(define (draw-note note speed)
  ; move the turtle in place
  (move-offset 30 (note 'height))
  ; draw the circle
  (make-note-circle 36)
  ; draw the appropriate note flag
  (draw-flag (note 'flag-pos-up?) speed)
  ; draw an extra staff line(s) as needed
  (draw-extra-staff-line (note 'name))
  ; fill in the note if needed
  (draw-fill-in speed)
  ; draw sharp symbol if note is sharp
  (draw-sharp (note 'sharp?))
  ; prepare to draw next note
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
          ; if eighth or sixteenth, we have more flag lines to draw
          (cond ((eq? speed eighth)
                 (eighth-flag-up))
                ((eq? speed sixteenth)
                 (sixteenth-flag-up))
                (else
                 (move-offset -5 37)))
        )
        (begin
          (move-offset -6 -7)
          (draw-offset 0 30)
          ; if eighth or sixteenth, we have more flag lines to draw
          (cond ((eq? speed eighth)
                 (eighth-flag-down))
                ((eq? speed sixteenth)
                 (sixteenth-flag-down))
                (else
                 (move-offset 6 -23)))
        )
    )
    (nothing)
  )
)

; draw a flag on the up eighth note
(define (eighth-flag-up)
  (draw-offset 10 0)
  (move-offset -15 37)
)

; draw a flag on the up sixteenth note
(define (sixteenth-flag-up)
  (eighth-flag-up)
  (move-offset 5 -33)
  (draw-offset 10 0)    ; TODO - more testing on this movement.
  (move-offset -15 33)
)

; draw a flag on the down eighth note
(define (eighth-flag-down)
  (draw-offset -10 0)
  (move-offset 16 -23)
)

; draw a flag on the down sixteenth note
(define (sixteenth-flag-down)
  (eighth-flag-down)
  (move-offset -6 19)
  (draw-offset -10 0)
  (move-offset 16 -19)
)

; draws the extra staff line if needed for the note
(define (draw-extra-staff-line name)
  ; extra line through the note
  (cond ((or (equal? name 'c4)
             (equal? name 'c#4)
             (equal? name 'e2)
             (equal? name 'a5)
             (equal? name 'a#5))
        (line-through))
      ; extra line below the note
      ((equal? name 'b5)
        (line-below))
      ; extra line above the note
      ((or (equal? name 'd2)
           (equal? name 'd#2))
        (line-above))
      ; extra line through and below the note
      ((equal? name 'c6)
        (begin
          (line-through)
          (line-far-below)
        ))
      ; extra line through and above the note
      ((or (equal? name 'c2)
           (equal? name 'c#2))
        (begin
          (line-through)
          (line-far-above)
        ))
      ; no extra line
      (else nothing)
  )
)

; draws a line through the note
(define (line-through)
  (move-offset 0 -5)
  (draw-offset 10 0)
  (move-offset -10 0)
  (draw-offset -12 0)
  (move-offset 12 5)
)

; draws a line below the note
(define (line-below)
  (draw-offset 10 0)
  (move-offset -10 0)
  (draw-offset -12 0)
  (move-offset 12 0)
)

; draws a line above the note
(define (line-above)
  (move-offset 0 -11)
  (draw-offset 10 0)
  (move-offset -10 0)
  (draw-offset -12 0)
  (move-offset 12 11)
)

; draws a line far below the note
(define (line-far-below)
  (move-offset 0 5)
  (draw-offset 10 0)
  (move-offset -10 0)
  (draw-offset -12 0)
  (move-offset 12 -5)
)

; draws a line far above the note
(define (line-far-above)
  (move-offset 0 -15)
  (draw-offset 10 0)
  (move-offset -10 0)
  (draw-offset -12 0)
  (move-offset 12 15)
)
  
; fill in the note if needed
(define (draw-fill-in speed)
  (if (or (equal? speed quarter)
          (equal? speed eighth)
          (equal? speed sixteenth))
      (begin
        (move-offset 0 -1)
        (fill-in-helper 5)
        (move-offset 0 1)
      )
      (nothing)
  )
)

(define (fill-in-helper level)
  (if (= level 0)
      (nothing)
      (begin
        (draw-offset level (- 0 level))
        (draw-offset (- 0 level) (- 0 level))
        (draw-offset (- 0 level) level)
        (draw-offset level level)
        (fill-in-helper (- level 1))
      )
  )
)

; draw the sharp on the note if needed
(define (draw-sharp is-sharp)
  (if is-sharp
      (begin
        (move-offset -14 -11)
        (draw-offset 0 10)
        (move-offset 4 0)
        (draw-offset 0 -10)
        (move-offset 2 2)
        (draw-offset -8 2)
        (move-offset 0 3)
        (draw-offset 8 -2)
        (move-offset 8 6)
      )
      (nothing)
  )
)

; helper function that does the actual drawing
(define (make-note-circle remaining)
  (turn 10)
  (draw 1)
  (if (= (- remaining 1) 0)
      (nothing)
      (make-note-circle (- remaining 1)))
)

; ease-of-use command to do nothing
(define (nothing)
  (turn 0)
)

; test function
(define (test)
  (turtles #t)
  (draw-new-staff)
  (draw-note (hash-ref note-with-name 'b3)  'whole)
  (draw-note (hash-ref note-with-name 'c#4) 'eighth)
  (draw-note (hash-ref note-with-name 'c2)  'quarter)
  (draw-note (hash-ref note-with-name 'd#2) 'sixteenth)
  (draw-note (hash-ref note-with-name 'e2)  'half)
  (draw-note (hash-ref note-with-name 'f#2) 'eighth)
  (draw-note (hash-ref note-with-name 'g2)  'sixteenth)
  (draw-note (hash-ref note-with-name 'c6)  'whole)
  (draw-note (hash-ref note-with-name 'b5)  'quarter)
  (draw-note (hash-ref note-with-name 'a#5) 'half)
  (draw-note (hash-ref note-with-name 'g5)  'whole)
  (draw-note (hash-ref note-with-name 'f5)  'eighth)
  (draw-note (hash-ref note-with-name 'f#5) 'sixteenth)
  (draw-note (hash-ref note-with-name 'f5)  'eighth)
  (draw-note (hash-ref note-with-name 'f#5) 'half)
  (draw-note (hash-ref note-with-name 'f5)  'eighth)
)

(define (big-test)
  (turtles #t)
  (test)
  (test)
  (test)
  (test)
  (prog-test)
  (disappear)
)

(define (prog-test)
  (turtles #t)
  (define chord1 (make-chord "d" "b" "a" "b" "c"))
  (define chord2 (make-chord "b" "b" "b" "b" "c"))
  (define chord3 (make-chord "c" "b" "b" "b" "c"))
  (define chord4 (make-chord "e" "b" "b" "b" "c"))
  (define progression (list chord1 chord2 chord3 chord4))
  (draw-progression progression)
  (disappear)
)

(define (clef-test)
  (turtles #t)
  (draw-new-staff)
  (draw-bass-clef)
  (disappear)
)
