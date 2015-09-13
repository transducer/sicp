#lang planet neil/sicp

; Consider the following make-cycle procedure, which uses the 
; last-pair procedure defined in exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair lst)
  (if (null? (cdr lst)) 
      lst
      (last-pair (cdr lst)))) 

; Draw a box-and-pointer diagram that shows the structure z 
; created by

(define z (make-cycle (list 'a 'b 'c)))

; What happens if we try to compute (last-pair z)? 

(last-pair z)
; => Non-halting loop...
