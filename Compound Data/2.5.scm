#lang planet neil/sicp

;; Show that we can represent pairs of nonnegative integers using
;; only numbers and arithmetic operations if we represent the 
;; pair a and b as the integer that is the product 2^a*3^b. Give 
;; the corresponding definitions of the procedures cons, car, and
;; cdr. 

;; Helpers
(define (power base exp)
  (define (iter base count result)
    (if (= count 0) result
        (iter base (- count 1) (* base result))))
  (iter base exp 1))

;; After looking at the solution the following helper seems not
;; to be necessary. Leave it for historical reasons.
(define (factor-of? factor n)
  (cond 
    [(= n factor) #t]
    [(< n factor) #f]
    (else (factor-of? factor (/ n factor)))))

;; An actual useful solution counts the time the number is
;; divisible by 2, the remainder will be divisible by 3.

; Count the number of times n is evenly divisible by d
(define (num-divs n d)
  (define (iter x result)
    (if (= 0 (remainder x d))
        (iter (/ x d) (+ 1 result))
        result))
  (iter n 0))

;; Testing the helpers:
(power 2 3) ;; => 8
(power 3 3) ;; => 27
(power 1 1) ;; => 1
(power 1 0) ;; => 1

(factor-of? 2 2)  ;; => #t
(factor-of? 2 8)  ;; => #t
(factor-of? 2 5)  ;; => #f
(factor-of? 3 6)  ;; => #f
(factor-of? 3 9)  ;; => #t
(factor-of? 3 10) ;; => #f

;; => Helpers work for positive integers.

;; Define the pair:
(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (cdr pair)
  (num-divs pair 3))

(define (car pair)
  (num-divs pair 2))
  
;; Testing
(cons 2 3) ;; => (2^2 * 3^3 = 4 * 27 =) 108
(car 108)  ;; => 2
(cdr 108)  ;; => 3


