#lang scheme

;; Implement the union-set operation for the unordered-list representation 
;; of sets. 

;; Import from the book:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; The union set is the set containing each element that appears in either 
;; argument.
(define (union-set a b) 
  (cond ((null? b) a) 
        ((element-of-set? (car b) a) (union-set a (cdr b))) 
        (else (cons (car b) (union-set a (cdr b)))))) 

;; Testing:
(define a (list 1 2 3))
(define b (list 3 4 5))

(union-set a b)
;; => '(4 5 1 2 3)
