#lang scheme

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  ; (display angle) (newline)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; How many times called for (sine 12.15)?
; => 5

; Order of growth and number of steps as function of a?
; => ??? O(log a)
