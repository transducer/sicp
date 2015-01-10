#lang scheme

;; J.H. Lambert tan-function:

;;                r
;; tan r = ----------------
;;         1 -     r^2
;;              -----------
;;              3 -   r^2
;;                  -------
;;                  5 - ...

(define (++ x) (+ x 1))

(define (cont-fract n d k) 
  (define (recur i) 
    (if (> i k) 
        0 
        (/ (n i) (+ (d i) (recur (++ i)))))) 
  (recur 1))

(define (lambert-tan x k)
  (cont-fract (lambda (i) (if (= i 1) x (* x x)))
              (lambda (i) (- (* 2 i) 1))
              k))

(lambert-tan 3.14159 10000) ;; => 0.99, should be 0.

;; This does not seem to be correct, but solution is similar.