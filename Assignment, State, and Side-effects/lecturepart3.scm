#lang scheme

; Note: notes of lecture part 2 unfortunately lost on old hard drive.

; Example of usages of the awful thing we introduced called state.

;;; Cesaro's method for estimating Pi:
;;;   Prob(gcds(n1, n2)=1) = 6/(Pi*Pi)

(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define 1+
  (lambda (n) (+ n 1)))

(define 1-
  (lambda (n) (- n 1)))

(define (monte-carlo trials experiment)
  (let loop
    ((remaining trials)
     (passed 0))
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (loop (1- remaining)
                 (1+ passed)))
          (else
           (loop (1- remaining)
                 passed)))))

; Example from lecture:
;(define rand
;  (let ((x random-init))
;    (lambda ()
;      (set! x (rand-update x))
;      x))) ; x is hidden piece of local state internal to the rand operation.

; Working implementation using Scheme function:
(define rand
  (lambda () (floor (* 10000 (random)))))

(estimate-pi 1000000)
; => 3.14...

; Writing the program without assignments makes the random number generator leak out of 
; cicero and monte-carlo (you need to pass the seed of the random number generator).
; This makes monte-carlo no longer general, it knows how many random numbers you need.

; I missed the ability to make a program modular by having a state variable that is confined
; to an object.

; This is enough reason for introducing state.


