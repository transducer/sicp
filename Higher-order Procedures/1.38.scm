#lang scheme

;; In 1737, the Swiss mathematician Leonhard Euler published a 
;; memoir De Fractionibus Continuis, which included a continued 
;; fraction expansion for e - 2, where e is the base of the 
;; natural logarithms. In this fraction, the Ni are all 1, and 
;; the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... 
;; Write a program that uses your cont-frac procedure from 
;; exercise 1.37 to approximate e, based on Euler's expansion. 

(define (inc x) (+ x 1))
(define (cont-fract n d k) 
  (define (recur i) 
    (if (> i k) 
        0 
        (/ (n i) (+ (d i) (recur (inc i)))))) 
  (recur 1))

;; Logic is:
;; 2 = 2
;; 5 = 4
;; 8 = 6
;; 11 = 8
;; 14 = 10
;; 17 = 12
;; So every
(define e
  (cont-fract (lambda (i) 1.0) 
              (lambda (i) 
                (if (= (remainder i 3) 2) 
                    (* (+ i 1) (/ 2 3)) 
                    1)) 
              10))

e ;; => 0.7182817182817183