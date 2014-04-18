; NOTE - remove this line when ready to port over.  
; Uncomment when developing to avoid errors
#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SchArpeggio                                          ;;;
;;; Objects File                                         ;;;
;;; Mike Stowell and Jeremy Poulin                       ;;;
;;;                                                      ;;;
;;; Class that contains all the major objects that must  ;;;
;;; be shared between the driver, music library, and     ;;;
;;; drawing library                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #region Objects by Mike

; the message passing style chord-config object
(define (make-chord-config chord option speed flavor range)
  ; chord configuration
  (define (get-chord) chord)
  (define (set-chord arg) (set! chord arg))
  
  ; option to move the chord
  (define (get-option) option)
  (define (set-option arg) (set! option arg))
  
  ; speed to play the arpeggio
  (define (get-speed) speed)
  (define (set-speed arg) (set! speed arg))
  
  ; flavor of the arpeggio
  (define (get-flavor) flavor)
  (define (set-flavor arg) (set! flavor arg))
  
  ; range to allow the notes to arpeggiate over
  (define (get-range) range)
  (define (set-range arg) (set! range arg))
  
  ; dispatch procedure
  ; if arg == null, then we're requesting a getter
  ; otherwise, we want to use a setter with that argument
  (define (dispatch sym . arg)
    (if (null? arg)
        (cond ((eq? sym 'chord) (get-chord))
          ((eq? sym 'option) (get-option))
          ((eq? sym 'speed) (get-speed))
          ((eq? sym 'flavor) (get-flavor))
          ((eq? sym 'range) (get-range))
          (else (error 
                 (string-append "NO SUCH GETTER METHOD: " 
                                (symbol->string sym))))
         )
        (cond ((eq? sym 'chord) (set-chord arg))
          ((eq? sym 'option) (set-option arg))
          ((eq? sym 'speed) (set-speed arg))
          ((eq? sym 'flavor) (set-flavor arg))
          ((eq? sym 'range) (set-range arg))
          (else (error 
                 (string-append "NO SUCH SETTER METHOD: " 
                                (symbol->string sym))))
       )
    )
  )
  dispatch
)

; convert the string input to our internal chord list
(define (string->chord str)
  (map note-sym-to-chord-obj 
    (map string->symbol 
       (map notes-to-g3-through-f4 
            (string-split str))))
)

(define (note-sym-to-chord-obj sym)
   (hash-ref note-with-name sym)
)
  
; convert notes to appropriate representation
(define (notes-to-g3-through-f4 str)
  (cond ((equal? str "a") "a4")
        ((equal? str "b") "b4")
        ((equal? str "c") "c4")
        ((equal? str "d") "d4")
        ((equal? str "e") "e4")
        ((equal? str "f") "f4")
        (else             "g3")
  )
)

; the message passing style chord object
(define (make-chord str)  
  ; the chord list
  (define chord (string->chord str))
  
  ; accessors for the chord
  (define (get-root-note) (car chord))
  (define (get-rest) (cdr chord))
  
  ; dispatch procedure
  (define (dispatch sym)
    (cond ((eq? sym 'root-note) (get-root-note))
          ((eq? sym 'other-notes) (get-rest))
          (else (error 
                 (string-append "NO SUCH METHOD ON CHORDS: " 
                                (symbol->string sym))))
    )
  )
  dispatch
)

; global values for the enums of option, speed, flavor, and range
(define down-to-up 'down-to-up)
(define up-to-down 'up-to-down)
(define random 'random)
(define whole 'whole)
(define half 'half)
(define fourth 'fourth)
(define eighth 'eighth)
(define sixteenth 'sixteenth)
(define major 'major)
(define minor 'minor)
(define root 'root)
(define dom 'dom)
(define third 'third)
(define high-root 'high-root)
(define low-dom 'low-dom)
(define low-third 'low-third)
(define low-root 'low-root)

; enum for option
(define (make-option in)
  (cond ((eq? in 'a) down-to-up)
        ((eq? in 'b) up-to-down)
        (else        random)
  )
)

; enum for speed
(define (make-speed in)
  (cond ((eq? in 'a) whole)
        ((eq? in 'b) half)
        ((eq? in 'c) fourth)
        ((eq? in 'd) eighth)
        (else        sixteenth)
  )
)

; enum for flavor
(define (make-flavor in)
  (if (eq? in 'a)
      major
      minor
  )
)

; enum for range
(define (make-range in)
  (cond ((eq? in 'a) root)
        ((eq? in 'b) dom)
        ((eq? in 'c) third)
        ((eq? in 'd) high-root)
        ((eq? in 'e) low-dom)
        ((eq? in 'f) low-third)
        (else        low-root)
  )
)

;; #endregion

;; #region Objects by Mike and Jeremy

; Note definition
(define (make-note name frequency position height sharp flag-pos-up)
  (define (dispatch msg)
    (cond ((eq? msg 'name) name)
          ((eq? msg 'freq) frequency)
          ((eq? msg 'position) position)
          ((eq? msg 'height) height)
          ((eq? msg 'sharp?) sharp)
          ((eq? msg 'flag-pos-up?) flag-pos-up)
          (else (error 
                 (string-append "NO SUCH NOTE METHOD: " 
                                (symbol->string msg))))))
  dispatch)

; Global hash set of notes -----------------------------------

; Mark the position of each note relative to c4
(define (make-position-itr)
  (define n -12.5)
  (define (dispatch . msg)
    (cond ((and (pair? msg) (eq? (car msg) 'reset)) (set! n -12.5))
          (else (begin (set! n (+ n 0.5))
                       n))))
  dispatch)
(define cur-position (make-position-itr))

; Mike - give notes a draw height
(define (make-draw-height-itr)
  (define n 145)
  (define (dispatch . msg)
    (cond ((and (pair? msg) (eq? (car msg) 'sharp)) n)
          (else (begin (set! n (- n 5))
                       n))))
  dispatch)
(define cur-draw-height (make-draw-height-itr))

; Notes themselves (Jer and Mike)
(define note-with-name (hash
                'c2 (make-note  'c2    65.40 (cur-position) (cur-draw-height)        #f #t)
               'c#2 (make-note 'c#2    69.29 (cur-position) (cur-draw-height 'sharp) #t #t)
                'd2 (make-note  'd2    73.41 (cur-position) (cur-draw-height)        #f #t)
               'd#2 (make-note 'd#2    77.78 (cur-position) (cur-draw-height 'sharp) #t #t)
                'e2 (make-note  'e2    82.40 (cur-position) (cur-draw-height)        #f #t)
                'f2 (make-note  'f2    87.30 (cur-position) (cur-draw-height)        #f #t)
               'f#2 (make-note 'f#2    92.49 (cur-position) (cur-draw-height 'sharp) #t #t)
                'g2 (make-note  'g2    97.99 (cur-position) (cur-draw-height)        #f #t)
               'g#2 (make-note 'g#2   103.82 (cur-position) (cur-draw-height 'sharp) #t #t)
                'a2 (make-note  'a2   110.00 (cur-position) (cur-draw-height)        #f #t)
               'a#2 (make-note 'a#2   116.54 (cur-position) (cur-draw-height 'sharp) #t #t)
                'b2 (make-note  'b2   123.47 (cur-position) (cur-draw-height)        #f #t)
                'c3 (make-note  'c3   130.81 (cur-position) (cur-draw-height)        #f #t)
               'c#3 (make-note 'c#3   138.59 (cur-position) (cur-draw-height 'sharp) #t #t)
                'd3 (make-note  'd3   146.83 (cur-position) (cur-draw-height)        #f #f)
               'd#3 (make-note 'd#3   155.56 (cur-position) (cur-draw-height 'sharp) #t #f)
                'e3 (make-note  'e3   164.81 (cur-position) (cur-draw-height)        #f #f)
                'f3 (make-note  'f3   174.61 (cur-position) (cur-draw-height)        #f #f)
               'f#3 (make-note 'f#3   184.99 (cur-position) (cur-draw-height 'sharp) #t #f)
                'g3 (make-note  'g3   195.99 (cur-position) (cur-draw-height)        #f #f)
               'g#3 (make-note 'g#3   207.65 (cur-position) (cur-draw-height 'sharp) #t #f)
                'a3 (make-note  'a3   220.00 (cur-position) (cur-draw-height)        #f #f)
               'a#3 (make-note 'a#3   233.08 (cur-position) (cur-draw-height 'sharp) #t #f)
                'b3 (make-note  'b3   246.94 (cur-position) (cur-draw-height)        #f #f)
                'c4 (make-note  'c4   261.62 (cur-position) (cur-draw-height)        #f #f)
               'c#4 (make-note 'c#4   277.18 (cur-position) (cur-draw-height 'sharp) #t #f)
                'd4 (make-note  'd4   293.66 (cur-position) (cur-draw-height)        #f #f)
               'd#4 (make-note 'd#4   311.12 (cur-position) (cur-draw-height 'sharp) #t #f)
                'e4 (make-note  'e4   329.63 (cur-position) (cur-draw-height)        #f #t)
                'f4 (make-note  'f4   349.22 (cur-position) (cur-draw-height)        #f #t)
               'f#4 (make-note 'f#4   369.99 (cur-position) (cur-draw-height 'sharp) #t #t)
                'g4 (make-note  'g4   391.99 (cur-position) (cur-draw-height)        #f #t)
               'g#4 (make-note 'g#4   415.30 (cur-position) (cur-draw-height 'sharp) #t #t)
                'a4 (make-note  'a4   440.00 (cur-position) (cur-draw-height)        #f #t)
               'a#4 (make-note 'a#4   466.16 (cur-position) (cur-draw-height 'sharp) #t #t)
                'b4 (make-note  'b4   493.88 (cur-position) (cur-draw-height)        #f #t)
                'c5 (make-note  'c5   523.25 (cur-position) (cur-draw-height)        #f #f)
               'c#5 (make-note 'c#5   554.36 (cur-position) (cur-draw-height 'sharp) #t #f)
                'd5 (make-note  'd5   587.33 (cur-position) (cur-draw-height)        #f #f)
               'd#5 (make-note 'd#5   622.25 (cur-position) (cur-draw-height 'sharp) #t #f)
                'e5 (make-note  'e5   659.25 (cur-position) (cur-draw-height)        #f #f)
                'f5 (make-note  'f5   698.45 (cur-position) (cur-draw-height)        #f #f)
               'f#5 (make-note 'f#5   739.98 (cur-position) (cur-draw-height 'sharp) #t #f)
                'g5 (make-note  'g5   783.99 (cur-position) (cur-draw-height)        #f #f)
               'g#5 (make-note 'g#5   830.60 (cur-position) (cur-draw-height 'sharp) #t #f)
                'a5 (make-note  'a5   880.00 (cur-position) (cur-draw-height)        #f #f)
               'a#5 (make-note 'a#5   932.32 (cur-position) (cur-draw-height 'sharp) #t #f)
                'b5 (make-note  'b5   987.76 (cur-position) (cur-draw-height)        #f #f)
                'c6 (make-note  'c6  1046.50 (cur-position) (cur-draw-height)        #f #f)
                )
)

;; #endregion

;; #region Objects by Jeremy

; Relative positions
(cur-position 'reset)
(define note-at-position
  (hash (cur-position)  'c2
        (cur-position) 'c#2
        (cur-position)  'd2
        (cur-position) 'd#2
        (cur-position)  'e2
        (cur-position)  'f2
        (cur-position) 'f#2
        (cur-position)  'g2
        (cur-position) 'g#2
        (cur-position)  'a2
        (cur-position) 'a#2
        (cur-position)  'b2
        (cur-position)  'c3
        (cur-position) 'c#3
        (cur-position)  'd3
        (cur-position) 'd#3
        (cur-position)  'e3
        (cur-position)  'f3
        (cur-position) 'f#3
        (cur-position)  'g3
        (cur-position) 'g#3
        (cur-position)  'a3
        (cur-position) 'a#3
        (cur-position)  'b3
        (cur-position)  'c4
        (cur-position) 'c#4
        (cur-position)  'd4
        (cur-position) 'd#4
        (cur-position)  'e4
        (cur-position)  'f4
        (cur-position) 'f#4
        (cur-position)  'g4
        (cur-position) 'g#4
        (cur-position)  'a4
        (cur-position) 'a#4
        (cur-position)  'b4
        (cur-position)  'c5
        (cur-position) 'c#5
        (cur-position)  'd5
        (cur-position) 'd#5
        (cur-position)  'e5
        (cur-position)  'f5
        (cur-position) 'f#5
        (cur-position)  'g5
        (cur-position) 'g#5
        (cur-position)  'a5
        (cur-position) 'a#5
        (cur-position)  'b5
        (cur-position)  'c6))

;; #endregion
