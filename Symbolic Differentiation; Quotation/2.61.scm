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


;; Faster adjoin: just use improved element-of-set? ?
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; Testing:
(define a 6)
(define b (list 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

(time (adjoin-set a b))
;; => (2 3 4 5)
