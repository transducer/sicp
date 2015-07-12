#lang scheme

;; Give a O(n) implementation of union-set for sets represented as ordered 
;; lists. 

;; Import from the book:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; From solutions:
(define (union-set set1 set2) 
  (cond  ((null? set1) set2) 
         ((null? set2) set1) 
         ((= (car set1) (car set2))  
          (cons (car set1) (union-set (cdr set1) (cdr set2)))) 
         ((< (car set1) (car set2))   
          (cons (car set1) (union-set (cdr set1) set2))) 
         (else  
          (cons (car set2) (union-set set1 (cdr set2)))))) 


;; Testing
(define a (list 3 4 5))
(define b (list 1 2))

(union-set a b)
;; => (2 3 2 1 3 2 2)


