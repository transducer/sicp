#lang scheme

;; Exercise 3.69 discussed how to generate the stream of all Pythagorean triples, with no 
;; upper bound on the size of the integers to be searched. Explain why simply replacing 
;; an-integer-between by an-integer-starting-from in the procedure in exercise 4.35 is not 
;; an adequate way to generate arbitrary Pythagorean triples. Write a procedure that 
;; actually will accomplish this. (That is, write a procedure for which repeatedly typing 
;; try-again would in principle eventually generate all Pythagorean triples.)

; We would get something like:

(define (a-pythagorean-triple-from n)
  (let ((i (an-integer-starting-from n)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; This would to infinite searches since there is no upper bound, which is needed for the
; recursive backtracking to terminate.

; Something that would work is this (source: http://community.schemewiki.org/?sicp-ex-4.36):

(define (a-pythagorean-triple-greater-than low) 
  (let ((k (an-integer-starting-from low))) 
    (let ((i (an-integer-between low k))) 
      (let ((j (an-integer-between i k))) 
        (require (= (+ (* i i) (* j j)) (* k k))) 
        (list i j k))))) 

