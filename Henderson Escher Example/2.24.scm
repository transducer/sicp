#lang racket

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(list 1 (list 2 (list 3 4)))

;; Give the result printed by the interpreter,

;; '(1 (2 (3 4)))

;; the corresponding box-and-pointer structure, 

;; [1|-]-> [2|-]-> [3| 4]

;; and the interpretation of this as a tree.

;;      1
;;      |
;;      2     
;;      / \    
;;     3    4

;; From answer:

;; (1 (2 (3 4)))
;;      ^
;;    /   \
;;   1     ^ (2 (3 4))
;;       /   \
;;      2     ^ (3 4)
;;          /   \
;;         3     4

;;   +---+---+  +---+---+
;;   | * | *-+->| * | / |
;;   +-+-+---+  +-+-+---+
;;     |          |   
;;     V          V      
;;   +---+      +---+---+  +---+---+
;;   | 1 |      | * | *-+->| * | / |
;;   +---+      +-+-+---+  +---+---+
;;                |          |
;;                V          V
;;              +---+      +---+---+  +---+---+
;;              | 2 |      | * | *-+->| * | / |
;;              +---+      +-+-+---+  +-+-+---+
;;                           |          |
;;                           V          V
;;                         +---+      +---+
;;                         | 3 |      | 4 |
;;                         +---+      +---+