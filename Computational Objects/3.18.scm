#lang planet neil/sicp

; Write a procedure that examines a list and determines whether it contains 
; a cycle, that is, whether a program that tried to find the end of the list
; by taking successive cdrs would go into an infinite loop. Exercise 3.13 
; constructed such lists.

(define (has-cycle? lst) 
  (let detect ((l lst) 
               (counted-list '())) 
    (cond ((not (pair? l)) #f) 
          ((memq l counted-list) #t) 
          (else (detect (cdr l) (cons l counted-list))))))


; Testing:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair lst)
  (if (null? (cdr lst)) 
      lst
      (last-pair (cdr lst)))) 

(define lst (list 'a 'b 'c))
(has-cycle? lst)
; => #f

(define cycle (make-cycle lst))
(has-cycle? cycle)
; => #t
