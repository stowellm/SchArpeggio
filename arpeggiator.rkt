;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SchArpeggio                                          ;;;
;;; Music Library                                        ;;;
;;; Jeremy Poulin                                        ;;;
;;;                                                      ;;;
;;; Class that contains all the musical magic that       ;;;
;;; goes on in the arpeggiator.                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;#lang racket
;(require racket/include)
;(include "objects.rkt")

;; start up SuperCollider server with: scsynth -u 57110 -D 0
; start the server's node list
(display "Super Collider initialized: ")
(with-sc3 (lambda (fd) (send fd (g-new1 1 add-to-tail 0))))

;;;;;; Notes to self ;;;;;;;
; out is function with 2 params       :channel :sound
; mul is function with 2 params       :sound   :volume
; sin-osc is a function with 3 params :func    :freq    :phase-offset
;

;; reset
(define (rsc3-stop) (with-sc3 reset))

; balance out the tone between earbuds
(define (even-out length note sound)
  (if (and (audition (out 0 (mul sound .25)))
           (audition (out 1 (mul sound .25))))
      (begin
        (sleep length)
        (rsc3-stop)
        note)
      #f))

; loops through chords objects and plays them sucessfully
(define (play-chord-progression prog)
  (play-chord (car prog))
  (play-chord-progression (append (cdr prog) (list (car prog)))))


; plays a chord with a given chord object
(define (play-chord c)
  (let ([num-notes (length (c 'notes))]
        [length (hash-ref note-length (c 'speed))])
    (play-note-in-chord c (modulo 0 num-notes))
    (if (< length (hash-ref note-length whole))
        (begin (play-note-in-chord c (modulo 1 num-notes))
               (if (< length (hash-ref note-length half))
                   (begin
                     (play-note-in-chord c (modulo 2 num-notes))
                     (play-note-in-chord c (modulo 3 num-notes))
                     (if (< length (hash-ref note-length quarter))
                         (begin
                           (play-note-in-chord c (modulo 4 num-notes))
                           (play-note-in-chord c (modulo 5 num-notes))
                           (play-note-in-chord c (modulo 6 num-notes))
                           (play-note-in-chord c (modulo 7 num-notes))
                           (if (< length (hash-ref note-length eighth))
                               (begin
                                 (play-note-in-chord c (modulo 8 num-notes))
                                 (play-note-in-chord c (modulo 9 num-notes))
                                 (play-note-in-chord c (modulo 10 num-notes))
                                 (play-note-in-chord c (modulo 11 num-notes))
                                 (play-note-in-chord c (modulo 12 num-notes))
                                 (play-note-in-chord c (modulo 13 num-notes))
                                 (play-note-in-chord c (modulo 14 num-notes))
                                 (play-note-in-chord c (modulo 15 num-notes)))
                               #t))
                         #t))
                   #t))
        #t)))

;; play note from a chord
(define (play-note-in-chord chord note-ref)
  (draw-note (list-ref (chord 'notes) note-ref) (chord 'speed))
  (define sound-func
    (cond ((eq? (chord 'sound-font) triangle-wave) lf-tri)
          ((eq? (chord 'sound-font) saw-wave) lf-saw)
          (else sin-osc)))
  (even-out
   (hash-ref note-length (chord 'speed))
   ((list-ref (chord 'notes) note-ref) 'name)
   (sound-func
    ar
    ((hash-ref note-with-name ((list-ref (chord 'notes) note-ref) 'name)) 'freq) 0)))

