#lang scheme

;; Define a procedure cont-frac such that evaluating 
;; (cont-frac n d k) computes the value of the k-term finite 
;; continued fraction.

(define (inc x) (+ x 1))

;; Multiplication by 1 / (1+1/D)
(define (cont-fract n d k) 
  (define (recur i) 
    (if (> i k) 
        0 
        (/ (n i) (+ (d i) (recur (inc i)))))) 
  (recur 1))

(cont-fract (lambda (i) 1.0) 
            (lambda (i) 1.0) 
            10) 
;; => 0.6179775280898876

;; Make it iterative.
(define (cont-fract-iter n d k) 
  (define (cont-fract-iter i result) 
    (if (= i 0) 
        result 
        (cont-fract-iter (- i 1)  
                        (/ (n i) (+ (d i) result))))) 
  (cont-fract-iter k 0.0)) 

(cont-fract-iter (lambda (i) 1.0) 
                 (lambda (i) 1.0) 
                 10) 
;; => 0.6179775280898876

