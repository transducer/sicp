#lang scheme

;; Implement the union-set operation for the unordered-list representation 
;; of sets. 

;; Import from the book:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; The union set is the set containing each element that appears in either 
;; argument (no duplicates):
(define (union-set a b) 
  (cond ((null? b) a) 
        ((element-of-set? (car b) a) (union-set a (cdr b))) 
        (else (cons (car b) (union-set a (cdr b)))))) 

;; Easy solution with filter and append:
(define (union-set-with-filter a b) 
  (append a (filter (lambda (x) (not (element-of-set? x a))) b))) 

;; Testing:
(define a (list 1 2 3))
(define b (list 3 4 5))

(union-set a b)
;; => (4 5 1 2 3)
(union-set-with-filter a b)
;; => (1 2 3 4 5)
