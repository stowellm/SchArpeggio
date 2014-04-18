#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SchArpeggio                                          ;;;
;;; Music Library                                        ;;;
;;; Jeremy Poulin                                        ;;;
;;;                                                      ;;;
;;; Class that contains all the musical magic that       ;;;
;;; goes on in the arpeggiator.                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rsc3)
(require racket/include)
(include "objects.rkt")

;; start up SuperCollider server with: scsynth -u 57110 -D 0
; start the server's node list
(with-sc3 (lambda (fd) (send fd (g-new1 1 add-to-tail 0))))

;;;;;; Notes to self ;;;;;;;
; out is function with 2 params       :channel :sound
; mul is function with 2 params       :sound   :volume
; sin-osc is a function with 3 params :func    :freq    :phase-offset
;

;; play sin wave
(define (play-note note . chord)
  (let ([play-note (lambda (n)
                    (if (eq? n '())
                        #f
                        (even-out 
                         n 
                         (sin-osc 
                          ar  
                          ((hash-ref note-with-name n) 'freq) 0))))])
    (map play-note (cons note chord))))

; balance out the tone between earbuds
(define (even-out note sound)
  (if (and (audition (out 0 (mul sound .25)))
           (audition (out 1 (mul sound .25))))
      note
      #f))


; Note definition (Jer + Mike)
(define (make-note note frequency position height sharp flag-pos-up)
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
  (define (dispatch)
    (cond ((and (pair? msg) (eq? (car msg) 'sharp)) n)
          (else (begin (set! n (- n 5))
                       n))))
  dispatch)
(define cur-draw-height (make-draw-height-itr))

; Mike - determine if this note has a flag up
(define (has-flag-up? note)
  (let ((height ((hash-ref note-with-name note) 'height)))
    (cond ((and (> height 100) (< height 145) #f))
          ((and (> height 75)  (< height 105) #t))
          ((and (> height 35)  (< height 80) #f))
          (else #t)
    )
  )
)

; Notes themselves (Jer and Mike)
(define note-with-name (hash
                'c2 (make-note  'c2    65.40 (cur-position) (cur-draw-height)        #f (has-flag-up? 'c2))
               'c#2 (make-note 'c#2    69.29 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'c#2))
                'd2 (make-note  'd2    73.41 (cur-position) (cur-draw-height)        #f (has-flag-up? 'd2))
               'd#2 (make-note 'd#2    77.78 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'd#2))
                'e2 (make-note  'e2    82.40 (cur-position) (cur-draw-height)        #f (has-flag-up? 'e2))
                'f2 (make-note  'f2    87.30 (cur-position) (cur-draw-height)        #f (has-flag-up? 'f2))
               'f#2 (make-note 'f#2    92.49 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'f#2))
                'g2 (make-note  'g2    97.99 (cur-position) (cur-draw-height)        #f (has-flag-up? 'g2))
               'g#2 (make-note 'g#2   103.82 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'g#2))
                'a2 (make-note  'a2   110.00 (cur-position) (cur-draw-height)        #f (has-flag-up? 'a2))
               'a#2 (make-note 'a#2   116.54 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'a#2))
                'b2 (make-note  'b2   123.47 (cur-position) (cur-draw-height)        #f (has-flag-up? 'b2))
                'c3 (make-note  'c3   130.81 (cur-position) (cur-draw-height)        #f (has-flag-up? 'c3))
               'c#3 (make-note 'c#3   138.59 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'c#3))
                'd3 (make-note  'd3   146.83 (cur-position) (cur-draw-height)        #f (has-flag-up? 'd3))
               'd#3 (make-note 'd#3   155.56 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'd#3))
                'e3 (make-note  'e3   164.81 (cur-position) (cur-draw-height)        #f (has-flag-up? 'e3))
                'f3 (make-note  'f3   174.61 (cur-position) (cur-draw-height)        #f (has-flag-up? 'f3))
               'f#3 (make-note 'f#3   184.99 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'f#3))
                'g3 (make-note  'g3   195.99 (cur-position) (cur-draw-height)        #f (has-flag-up? 'g3))
               'g#3 (make-note 'g#3   207.65 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'g#3))
                'a3 (make-note  'a3   220.00 (cur-position) (cur-draw-height)        #f (has-flag-up? 'a3))
               'a#3 (make-note 'a#3   233.08 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'a#3))
                'b3 (make-note  'b3   246.94 (cur-position) (cur-draw-height)        #f (has-flag-up? 'b3))
                'c4 (make-note  'c4   261.62 (cur-position) (cur-draw-height)        #f (has-flag-up? 'c4))
               'c#4 (make-note 'c#4   277.18 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'c#4))
                'd4 (make-note  'd4   293.66 (cur-position) (cur-draw-height)        #f (has-flag-up? 'd4))
               'd#4 (make-note 'd#4   311.12 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'd#4))
                'e4 (make-note  'e4   329.63 (cur-position) (cur-draw-height)        #f (has-flag-up? 'e4))
                'f4 (make-note  'f4   349.22 (cur-position) (cur-draw-height)        #f (has-flag-up? 'f4))
               'f#4 (make-note 'f#4   369.99 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'f#4))
                'g4 (make-note  'g4   391.99 (cur-position) (cur-draw-height)        #f (has-flag-up? 'g4))
               'g#4 (make-note 'g#4   415.30 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'g#4))
                'a4 (make-note  'a4   440.00 (cur-position) (cur-draw-height)        #f (has-flag-up? 'a4))
               'a#4 (make-note 'a#4   466.16 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'a#4))
                'b4 (make-note  'b4   493.88 (cur-position) (cur-draw-height)        #f (has-flag-up? 'b4))
                'c5 (make-note  'c5   523.25 (cur-position) (cur-draw-height)        #f (has-flag-up? 'c5))
               'c#5 (make-note 'c#5   554.36 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'c#5))
                'd5 (make-note  'd5   587.33 (cur-position) (cur-draw-height)        #f (has-flag-up? 'd5))
               'd#5 (make-note 'd#5   622.25 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'd#5))
                'e5 (make-note  'e5   659.25 (cur-position) (cur-draw-height)        #f (has-flag-up? 'e5))
                'f5 (make-note  'f5   698.45 (cur-position) (cur-draw-height)        #f (has-flag-up? 'f5))
               'f#5 (make-note 'f#5   739.98 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'f#5))
                'g5 (make-note  'g5   783.99 (cur-position) (cur-draw-height)        #f (has-flag-up? 'g5))
               'g#5 (make-note 'g#5   830.60 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'g#5))
                'a5 (make-note  'a5   880.00 (cur-position) (cur-draw-height)        #f (has-flag-up? 'a5))
               'a#5 (make-note 'a#5   932.32 (cur-position) (cur-draw-height 'sharp) #t (has-flag-up? 'a#5))
                'b5 (make-note  'b5   987.76 (cur-position) (cur-draw-height)        #f (has-flag-up? 'b5))
                'c6 (make-note  'c6  1046.50 (cur-position) (cur-draw-height)        #f (has-flag-up? 'c6))
                )
)

; Postitions
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

;; reset
(define (stop) (with-sc3 reset))

