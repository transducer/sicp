#lang scheme

;; Suppose that (in ordinary applicative-order Scheme) we define unless as shown above 
;; and then define factorial in terms of unless as

; (define (factorial n)
;   (unless (= n 1)
;           (* n (factorial (- n 1)))
;           1))

;; What happens if we attempt to evaluate (factorial 5)? Will our definitions work in a 
;; normal-order language?

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)

; => Infinite loop.
;    This occurs because even if n = 1 the exceptional-value and thus factorial will be
;    called. In a normal-order language the calculation is delayed and this would work.

