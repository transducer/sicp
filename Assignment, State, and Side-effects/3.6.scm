#lang scheme

;; It is useful to be able to reset a random-number generator to produce a 
;; sequence starting from a given value. Design a new rand procedure that 
;; is called with an argument that is either the symbol generate or the 
;; symbol reset and behaves as follows: (rand 'generate) produces a new 
;; random number; ((rand 'reset) <new-value>) resets the internal state 
;; variable to the designated <new-value>. Thus, by resetting the state, 
;; one can generate repeatable sequences. These are very handy to have when 
;; testing and debugging programs that use random numbers. 

(define (rand m)
  (define (random-init) 5)
  (define (rand-update x) (+ x 1)) ; incrementing, AKA pseudo random
  (define x random-init)
  (define (dispatch)
    (cond ((eq? m 'generate) (set! x (rand-update x)))
          ((eq? m 'reset) (set! x 0))
          (else (error "RAND - invalid arg"))))
  dispatch)

(rand 'generate)