#lang planet neil/sicp

;; Here is an alternative procedural representation of pairs. For
;; this representation, verify that (car (cons x y)) yields x for
;; any objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; Verify:
(car (cons 1 2))        ;; =>  1
(car (cons -1 2))       ;; => -1
(car (cons (+ 1 2) 2))  ;; =>  3

;; Proof:
(car (cons x y)) 
(car (lambda (m) (m x y))) 
((lambda (m) (m x y)) (lambda (p q) p)) 
((lambda (p q) p) x y) 
x 