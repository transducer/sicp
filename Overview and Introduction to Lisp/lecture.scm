#lang scheme

(sqrt x)
  (define (average x y)
  (/ (+ x y) 2))
  (define (square x) (* x x))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x))
     .0001))
  (define (try guess x)
    (if (good-enough? guess x)
        guess
        (try (improve guess x) x)))
  (try 1 x))

(sqrt 2)
