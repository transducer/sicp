#lang scheme

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Show that the golden ratio (section 1.2.2) is a fixed point of
;; the transformation x -> 1 + 1/x, and use this fact to compute 
;; phi by means of the fixed-point procedure.

;; By definition.

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))

phi ;;=> 1.618...

;; Via solution phi derivation instead of the miracle above.

;; x = 1 + 1/x
;; x^2 = x + 1
;; x^2 - x - 1 = 0

;; Solving by the quadratic formula and discarding the negative 
;; root,

;; x = ( 1 + sqrt(5)) / 2, which is the definition of phi. 