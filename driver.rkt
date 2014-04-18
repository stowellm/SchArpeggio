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
(include "objects.rkt")
(include "drawing.rkt")
;(include "rsc3-sine-play.rkt")

;; #endregion


;; #region Running the driver

; list of chord objects - the progession
(define progression '())


; procedure to start the driver up
(define (run)
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
  
  (send-chord-config-music chord-config)
  (send-chord-config-draw chord-config)
)

; prompt the user for the chord
(define (ask-for-chord)
  (prompt-for-and-return 
   (string-append 
    "Please input a chord progression.\n"
    "Up to 4 chords are allowed.\n"
    "Enter the chords as lowercase letters separated by a space: ")
  )
)

; prompt the user for the option
(define (ask-for-option)
  (prompt-for-and-return 
   (string-append
    "\n\nPlease input an option to play the arpeggio.\n"
    "a) Down to Up\n"
    "b) Up to Down\n"
    "c) Random\n"
    "Select a, b, or c (lower case letters only): ")
  )
)

; prompt the user for the speed
(define (ask-for-speed)
  (prompt-for-and-return 
   (string-append
    "\n\nPlease input a speed for the arpeggio.\n"
    "a) whole\n"
    "b) half\n"
    "c) fourth\n"
    "d) eighth\n"
    "e) sixteenth\n"
    "Select a, b, c, d, or e (lower case letters only): ")
   )
)

; prompt the user for the flavor
(define (ask-for-flavor)
  (prompt-for-and-return 
   (string-append
    "\n\nPlease input a flavor for the chord.\n"
    "a) major\n"
    "b) minor\n"
    "Select a or b (lower case letters only): ")
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
    "Select a, b, c, d, e, f, or g (lower case letters only) :")
  )
)

; send the chord-config off to the music library
(define (send-chord-config-music chord-config)
  (display "TODO-BY-JEREMY-WITH-HIS-CODE\n")
)

; send the chord-config off to the drawing library
(define (send-chord-config-draw chord-config)
  (display "TODO-BY-MIKE-WITH-HIS-CODE\n")
)

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