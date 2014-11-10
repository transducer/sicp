#lang scheme

(define (fast-expt b n)
  
  (define (square x) (* x x))

  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 10 3)

; Design a procedure that evolves an iterative exponentiation
; process that uses successive squaring and uses a logarithmic 
; number of steps, as does fast-expt. (Hint: Using the 
; observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the 
; exponent n and the base b, an additional state variable a, and 
; define the state transformation in such a way that the product 
; a bn is unchanged from state to state. At the beginning of the 
; process a is taken to be 1, and the answer is given by the 
; value of a at the end of the process. In general, the technique
; of defining an invariant quantity that remains unchanged from 
; state to state is a powerful way to think about the design of 
; iterative algorithms.) 

; Answer from internet:
(define (fast-expt2 b n) 
  
  (define (square x) (* x x)) 
  
  (define (iter a b n) 
    (cond ((= n 0) a) 
          ((even? n) (iter a (square b) (/ n 2))) 
          (else (iter (* a b) b (- n 1))))) 
  
  (iter 1 b n)) 
