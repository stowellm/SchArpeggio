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


;; reset
(define (stop) (with-sc3 reset))

