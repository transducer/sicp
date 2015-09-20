#lang racket

; Using primitive multiplier, adder, and constant constraints, define a 
; procedure averager that takes three connectors a, b, and c as inputs and 
; establishes the constraint that the value of c is the average of the 
; values of a and b. 

(require racket/include)
(include "constraint-system.rkt")

(define (averager a b avg)
  (let ((add-a-b (make-connector))
        (divide-by-two (make-connector)))
    (adder a b add-a-b)
    (constant 0.5 divide-by-two)
    (multiplier add-a-b divide-by-two avg)
    'ok))


; Testing

(define A (make-connector))
(define B (make-connector))
(define AVG (make-connector))
(averager A B AVG)
; => 'ok

(probe "Average" AVG)
; => #<procedure:me>

(set-value! A 10 'a-value)
; => 'done
(set-value! B 20 'a-value)
; => 'done

; => Probe: Average = 15.0

(forget-value! B 'a-value)
; => 'done
(set-value! B 50 'a-value)
; => 'done

; => Probe: Average = 30.0
