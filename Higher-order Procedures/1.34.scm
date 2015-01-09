#lang scheme

;; Suppose we define the procedure

(define (f g)
  (g 2))

;; Then we have:
(define (square x) (* x x))
(f square) ;; => 4

(f (lambda (z) (* z (+ z 1)))) ;; => 6

;; What happens if we (perversely) ask the interpreter to 
;; evaluate the combination (f f)? Explain. 

(f f) ;; => application: not a procedure

;; Because: (f 2) leads to (2 2) calling itself.