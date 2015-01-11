#lang scheme

;; Define a procedure double that takes a procedure of one 
;; argument as argument and returns a procedure that applies the
;; original procedure twice. For example, if inc is a procedure 
;; that adds 1 to its argument, then (double inc) should be a 
;; procedure that adds 2. 

(define (double f)
  (lambda (x) (f (f x))))

;; Testing:

(define (inc x) (+ x 1))

(inc 1)          ;; => 2
((double inc) 1) ;; => 3

;; What value is returned by
;; (((double (double double)) inc) 5)?

(((double (double double)) inc) 5) ;; => 21

;; Since 2^4 is 16 and 16+5 = 21.
