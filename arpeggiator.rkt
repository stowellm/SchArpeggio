;#lang racket

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
(define (play-note length note . chord)
  (let ([play-note (lambda (n)
                    (if (eq? n '())
                        #f
                        (even-out
                         length
                         n 
                         (sin-osc 
                          ar  
                          ((hash-ref note-with-name n) 'freq) 0))))])
    (map play-note (cons note chord))))

;; reset
(define (stop) (with-sc3 reset))

; balance out the tone between earbuds
(define (even-out length note sound)
  (if (and (audition (out 0 (mul sound .25)))
           (audition (out 1 (mul sound .25))))
      (begin
        (sleep length)
        (stop)
        note)
      #f))

(define (play-chord-progression prog)
  (play-chord (car prog))
  (play-chord-progression (append (cdr prog) (list (car prog)))))

(define (play-chord c)
  (let ([num-notes (length (c 'notes))]
        [length (hash-ref note-length (c 'speed))])
    (thread (lambda ()
              (play-note-in-chord c (modulo 0 num-notes))
              )))
  (sleep 2))
  
;; play note from a chord
(define (play-note-in-chord chord note-ref)
  (even-out
   (hash-ref note-length (chord 'speed))
   ((list-ref (chord 'notes) note-ref) 'name)
   (sin-osc
    ar  
    ((hash-ref note-with-name ((list-ref (chord 'notes) note-ref) 'name)) 'freq) 0)))

;(define c (make-chord "c" 'a 'a 'a 'a))
;(play-chord-progression (list c))

