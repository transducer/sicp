#lang scheme

;; Define a procedure square-tree analogous to the square-list 
;; procedure of exercise 2.21. That is, square-list should 
;; behave as follows:

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;; ;; => (1 (4 (9 16) 25) (36 49))

;; Define square-tree both directly (i.e., without using any 
;; higher-order procedures) and also by using map and recursion. 

;; Directly:

(define (square-tree tree)
  (define (sq x) (* x x))
  (define nil '())
  (cond ((null? tree) nil)
        ((not (pair? tree)) (sq tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree (list 1 (list 4 2) 3 5))
;; => (1 (16 4) 9 25)

;; Map and recursion:
(define (square-tree-map tree)
  (define (sq x) (* x x))
  (map (lambda (t) 
         (if (pair? t) 
             (square-tree-map t)
             (sq t)))
       tree))

(square-tree-map (list 1 (list 4 2) 3 5))
