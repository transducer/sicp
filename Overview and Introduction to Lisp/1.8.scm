#lang scheme

(define (cbrt x)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (try guess x)
    (if (good-enough? guess x)
        guess
        (try (improve guess x) x)))
  (try 1.0 x))

(cbrt 8)
(cbrt 64)
(cbrt 24386298)

; => 2.000004911675504
;    4.000017449510739
;    289.9892901316334