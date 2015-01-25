#lang scheme

;; Write a procedure fringe that takes as argument a tree 
;; (represented as a list) and returns a list whose elements are 
;; all the leaves of the tree arranged in left-to-right order. 
;; For example,

;; (define x (list (list 1 2) (list 3 4)))

;; (fringe x)
;; (1 2 3 4)

;; (fringe (list x x))
;; (1 2 3 4 1 2 3 4)

;; Looking at solution after 30 minutes:
(define (fringe tree) 
  (define nil '()) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (list tree)) 
        (else (append (fringe (car tree)) (fringe (cdr tree)))))) 

;; Testing:
(define x (list (list 1 2) (list 3 4)))

(fringe x) ;; => (1 2 3 4)

(define (fringe-solution L) 
  (define (fringe-help L result) 
    (if (null? L) ; if at end of the branch 
        result 
        (if (list? L) ; if the element is a list, not a number 
            (fringe-help (car L) ; left branch 
                         (fringe-help (cdr L) result)) ; right branch 
            (cons L result)))) ; otherwise gather numbers into a list 
  (fringe-help L '())) 

(fringe-solution '((1 2) (3 4) (5 6 7 (8 9)))) ;; => (1 2 3 4 5 6 7 8 9) 
