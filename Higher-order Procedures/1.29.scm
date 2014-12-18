#lang scheme

;; Sum abstraction
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Definite integral numerical method
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Integral of cube from 0 to 1 is 1/4.
(define (cube x) (* x x x))
(integral cube 0 1 0.01) ;; => .24998750000000042
(integral cube 0 1 0.001) ;; => .249999875000001

;; Define a procedure that takes as arguments f, a, b, and n and 
;; returns the value of the integral, computed using Simpson's 
;; Rule. Use your procedure to integrate cube between 0 and 1 
;; (with n = 100 and n = 1000), and compare the results to those 
;; of the integral procedure shown above. 

;; factor = (/ (- b a) n)
;; inta->b =~ dx/3 (FIRST+4(sum of ODDs)+2(sum of EVENs)+LAST)

(define (simpson f a b n)
  (define dx (/ (- b a) n))
  (define (1+ n) (+ n 1))
  (define (yk k) (f (+ a (* dx k))))
  (define (factor k) 
    (* (cond ((or (= k 0) (= k n)) 1)
              ((odd? k) 4)
              (else 2))
        (yk k)))
    (* (/ dx 3) (sum factor 0 1+ n)))

(simpson cube 0 1 1) ;; => 1/3
(simpson cube 0 1 2) ;; => 1/4
(simpson cube 0 1 2000) ;; => 1/4
