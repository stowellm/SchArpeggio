; NOTE - remove this line when ready to port over.  
; Uncomment when developing to avoid errors
;#lang racket

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
  (map string->symbol 
       (map notes-to-g3-through-f4 
            (string-split str)))
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

;; #region Objects by Jeremy





;; #endregion

;; #region Objects by Mike and Jeremy





;; #endregion
