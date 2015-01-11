#lang scheme

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))

(define (iterative-improve good-enough? improve-guess)
  (lambda (x)
    (let ((xim (improve-guess x)))
      (if (good-enough?) x xim)
      xim
      ((iterative-improve good-enough? improve-guess) xim))))

(define (sqrt x) 
  ((iterative-improve   
    good-enough?
    (lambda (y) 
      (/ (+ (/ x y) y) 2))) 1.0))

(define (fixed-point f first-guess) 
  ((iterative-improve 
    good-enough?
    f) first-guess)) 


