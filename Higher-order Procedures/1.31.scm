#lang scheme

;; The sum procedure is only the simplest of a vast number of 
;; similar abstractions that can be captured as higher-order 
;; procedures. Write an analogous procedure called product that 
;; returns the product of the values of a function at points over
;; a given range. Show how to define factorial in terms of 
;; product. 

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; Test the result
(define (identity x) x)
(define (inc x) (+ x 1))

(product identity 1 inc 10) ;; => 3628800

;; Define factorial
(define (factorial x) (product identity 1 inc x))

;; Test the result
(factorial 10) ;; => 3628800

;; Also use product to compute approximations to pi with the
;; formula:
;;  pi    2 * 4 * 4 * 6 * 6 * 8
;;  -- =  ---------------------
;;  4     3 * 3 * 5 * 5 * 7 * 7

(define pi (product 
            
            
(define (product term a next b)
