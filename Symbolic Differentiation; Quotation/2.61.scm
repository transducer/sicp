#lang scheme

;; Give an implementation of adjoin-set using the ordered representation. 
;; By analogy with element-of-set? show how to take advantage of the  
;; ordering to produce a procedure that requires on the average about half  
;; as many steps as with the unordered representation. 

;; Import from the book:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


;; Ordered adjoin:
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))


;; Testing:
(define a 6)
(define b (list 3 4 5 7))

(adjoin-set a b)
;; => (3 4 5 6 7)
(adjoin-set a '())
;; => (6)
