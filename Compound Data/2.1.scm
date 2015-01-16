#lang planet neil/sicp

;; Define a better version of make-rat that handles both positive
;; and negative arguments. Make-rat should normalize the sign so
;; that if the rational number is positive, both the numerator 
;; and denominator are positive, and if the rational number is 
;; negative, only the numerator is negative. 

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat-improved n d)
  (cond 
    [(and (< n 0) (< d 0))
    (make-rat (abs n) (abs d))]
    [(< n 0)
     (make-rat n d)]
    [(< d 0)
     (make-rat (* n -1) (abs d))]
    [else (make-rat n d)]))

;; Testing
(make-rat-improved 1 -2)  ;; => (mcons -1 2)
(make-rat-improved -1 2)  ;; => (mcons -1 2)
(make-rat-improved -1 -2) ;; => (mcons  1 2)
(make-rat-improved 1 2)   ;; => (mcons  1 2)


