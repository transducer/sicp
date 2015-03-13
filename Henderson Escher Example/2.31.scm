#lang scheme

;; Abstract your answer to exercise 2.30 to produce a procedure 
;; tree-map with the property that square-tree could be defined as

;; (define (square-tree tree) (tree-map square tree))

(define (square x) (* x x))

(define (tree-map square tree)
  (map (lambda (t) 
         (if (pair? t) 
             (tree-map square t)
             (square t)))
       tree))

(define (square-tree tree) (tree-map square tree))

(square-tree (list 1 (list 4 2) 3 5))
;; => (1 (16 4) 9 25)
