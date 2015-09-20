#lang racket

; Louis Reasoner wants to build a squarer, a constraint device with two 
; terminals such that the value of connector b on the second terminal will 
; always be the square of the value a on the first terminal. He proposes 
; the following simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

; There is a serious flaw in this idea. Explain. 

(require racket/include)
(include "constraint-system.rkt")


; Testing

(define A (make-connector))
(define B (make-connector))
(squarer A B)
; => 'ok

(probe "Square" B)
; => #<procedure:me>

(set-value! B 10 'user)
; => 'done

; => Probe: Square = 100

; I don't see a serious flaw as long as you don't set B. Setting B is not
; in the requirements.
