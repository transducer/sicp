#lang scheme

;; If f is a numerical function and n is a positive integer, then
;; we can form the nth repeated application of f, which is 
;; defined to be the function whose value at x is 
;; f(f(...(f(x))...)). For example, if f is the function 
;; x -> x + 1, then the nth repeated application of f is the 
;; function x -> x + n. 
;; If f is the operation of squaring a number, then 
;; the nth repeated application of f is the function that raises
;; its argument to the 2^nth power. Write a procedure that takes 
;; as inputs a procedure that computes f and a positive integer 
;; n and returns the procedure that computes the nth repeated 
;; application of f. Your procedure should be able to be used as
;; follows:
;;
;; ((repeated square 2) 5) should return 625

;; Use the hint of using composition:
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 0) 
      (begin
        (- n 1)
        (compose f f))
      f))

;; Testing:
(define (square x) (* x x))

((repeated square 2) 5) ;; => 625

;; Looking at answer and stackoverflow 
;; shows that keeping state is not idiomatic scheme.

;; Better to do the following:

(define (repeated-improved f n) 
  (if (< n 1) 
      (lambda (x) x) 
      (compose f (repeated-improved f (- n 1))))) 

((repeated-improved square 2) 5) ;; => 625

