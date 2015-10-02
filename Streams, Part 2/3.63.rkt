#lang racket

;; Louis Reasoner asks why the sqrt-stream procedure was not written in the 
;; following more straightforward way, without the local variable guesses:

;(define (sqrt-stream x)
;  (cons-stream 1.0
;               (stream-map (lambda (guess)
;                             (sqrt-improve guess x))
;                           (sqrt-stream x))))

;; Alyssa P. Hacker replies that this version of the procedure is 
;; considerably less efficient because it performs redundant computation. 
;; Explain Alyssa's answer. 

sqrt-stream x is recalculated everytime in Louis Reasoner's version,

;; Would the two versions still differ in efficiency if our implementation 
;; of delay used only (lambda () <exp>) without using the optimization 
;; provided by memo-proc (section 3.5.1)? 

Yes. memo-proc has no influence on this recalculation. It is storing the
result of a different procedure than the individual calculations of stream 
values.

; Note: according to solution my second answer is not correct and removing
; memoization in delay would lead to the same performance.
