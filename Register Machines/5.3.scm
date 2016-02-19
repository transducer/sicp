#lang scheme

;; Design a machine to compute square roots using Newton's method, as described in
;; section 1.1.7:

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; Begin by assuming that good-enough? and improve operations are available as primitives.
;; Then show how to expand these in terms of arithmetic operations. Describe each version of
;; the sqrt machine design by drawing a data-path diagram and writing a controller
;; definition in the register-machine language. 

; http://community.schemewiki.org/?sicp-ex-5.3

(assign guess (const 1.0)) 
test-g 
(assign temp (op *) (reg guess) (reg guess)) 
(assign temp (op -) (reg temp) (reg x)) 
(test (op >) (reg temp) (const 0)) 
(branch (label iter)) 
(assign temp (op -) (const 0) (reg temp)) 
iter 
(test (op <) (reg temp) (const 0.001)) 
(branch (label sqrt-done)) 
(assign temp (op /) (reg x) (reg guess)) 
(assign temp (op +) (reg temp) (reg guess)) 
(assign guess (op /) (reg temp) (const 2)) 
(goto (label test-g)) 
sqrt-done
