#lang scheme

; Pascal's triangle:
; Edges are 1, and each number inside the triangle is the sum of 
; the two numbers above it.

; Write procedure to compute recursively:

(define (pascal col row) 
  (cond ((= col 0) 1) 
        ((= col row) 1) 
        (else (+ (pascal (- col 1) (- row 1))  
                 (pascal col ( - row 1))))))

(pascal 0 0) ; => 1
(pascal 1 1) ; => 1
(pascal 2 5) ; => 10