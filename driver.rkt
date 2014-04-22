#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SchArpeggio                                          ;;;
;;; Driver Program                                       ;;;
;;; Mike Stowell                                         ;;;
;;;                                                      ;;;
;;; Class that contains the main driver to run the       ;;;
;;; SchArpeggiator program in addition to the global     ;;;
;;; object types required by the several components of   ;;;
;;; the overall program.                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #region Requirements

(require racket/file)
(require racket/string)
(require racket/include)

; requirements for objects
(include "objects.rkt")

; requirements for music
(include "arpeggiator.rkt")

; requirements for drawing
;(require graphics/turtles)
;(include "drawing.rkt")


;; #endregion


;; #region Running the driver

; list of chord objects - the progession
(define progression '())

; number of chords in the progression
(define num-chords 0)

; procedure to start the driver up
(define (run)
  (display (string-append 
            "Welcome to SchArpeggio!\n"              
            "You will be asked to enter 1 to 4 chord configurations.\n"
            "The setup will guide you through this process.\n\n\n"))
  
  (get-new-chords)
  
  (send-chord-config-music progression)
  ;(send-chord-config-draw progression)
)

(define (get-new-chords)
  (set! num-chords (+ num-chords 1))
  
  (define chord  (ask-for-chord))
  (define option (ask-for-option))
  (define speed  (ask-for-speed))
  (define flavor (ask-for-flavor))
  (define range  (ask-for-range))
  
  (define chord-config (make-chord
                        chord 
                        option 
                        speed
                        flavor
                        range))
  
  (if (= num-chords 4)
      ; user hit max num chords per progression
      (add-to-progression chord-config)
      (begin 
        ; ask for additional chords
        (if (equal? (prompt-for-and-return (string-append
                  "\n\nWould you like to enter another chord?\n"
                  "Enter Y for yes, N for no: ")) "Y")
            (begin 
              ; if yes, prompt for another chord
              (add-to-progression chord-config)
              (display "\n\n")
              (get-new-chords))
            ; otherwise, just add the last chord to the progression
            (add-to-progression chord-config)
        )
      )
  )
)

; adds the chord to our list
(define (add-to-progression chord)
  (set! progression (append progression (list chord)))
)

; prompt the user for the chord
(define (ask-for-chord)
  (prompt-for-and-return 
   (string-append 
    "--- Chord #: " (number->string num-chords) " ---\n\n"
    "Please input a chord progression.\n"
    "Up to 4 chords are allowed.\n"
    "Enter the root chord as a single lowercase letter: ")
  )
)

; prompt the user for the option
(define (ask-for-option)
  (prompt-for-and-return 
   (string-append
    "\n\nPlease input an option to play the arpeggio.\n"
    "a) Down to Up\n"
    "b) Up to Down\n"
    "c) Random Order\n"
    "Select a choice (lower case letters only): ")
  )
)

; prompt the user for the speed
(define (ask-for-speed)
  (prompt-for-and-return 
   (string-append
    "\n\nPlease input a speed for the arpeggio.\n"
    "a) whole\n"
    "b) half\n"
    "c) quarter\n"
    "d) eighth\n"
    "e) sixteenth\n"
    "Select a choice (lower case letters only): ")
   )
)

; prompt the user for the flavor
(define (ask-for-flavor)
  (prompt-for-and-return 
   (string-append
    "\n\nPlease input a flavor for the chord.\n"
    "a) major\n"
    "b) minor\n"
    "Select a choice (lower case letters only): ")
   )
)

; prompt the user for the range
(define (ask-for-range)
  (prompt-for-and-return 
   (string-append
    "\n\nPlease input a range for the notes in the chord.\n"
    "a) root\n"
    "b) dom\n"
    "c) third\n"
    "d) high root\n"
    "e) low dom\n"
    "f) low third\n"
    "g) low root\n"
    "Select a choice (lower case letters only): ")
  )
)

; send the chord-config off to the music library
(define (send-chord-config-music chord-config)
  (play-chord-progression chord-config)
)

; send the chord-config off to the drawing library
;(define (send-chord-config-draw chord-config)
  ; turn on the drawing board
  ;(turtles #t)
  ; draw the chord progression
  ;(draw-progression progression)
;)

;; #endregion

;; #region User input

; generic prompt procedure to print a message and ask for input
(define (prompt-for-and-return msg)
  (display msg)
  (read-line my-in-port)
)

; input port
(define my-in-port (current-input-port))

;; #endregion

; test code (remove later)
;(turtles #t)
;(draw-new-staff)
;(draw-note (hash-ref note-with-name 'c2))
;(draw-note (hash-ref note-with-name 'd2))
;(draw-note (hash-ref note-with-name 'e2))
;(draw-note (hash-ref note-with-name 'f2))
;(draw-note (hash-ref note-with-name 'g2))
;(draw-note (hash-ref note-with-name 'c6))
;(draw-note (hash-ref note-with-name 'b5))
;(draw-note (hash-ref note-with-name 'a5))
;(draw-note (hash-ref note-with-name 'g5))
;(draw-note (hash-ref note-with-name 'f5))

;(run)

;(define c (make-chord "c" "a" "a" "a" "a"))
(run)
