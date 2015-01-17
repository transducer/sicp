#lang planet neil/sicp

;; Use dot notation to write a procedure same-parity that takes 
;; one or more integers and returns a list of all the arguments 
;; that have the same even-odd parity as the first argument. For 
;; example,

;; (same-parity 1 2 3 4 5 6 7)
;; => (1 3 5 7)

;; (same-parity 2 3 4 5 6 7)
;; => (2 4 6)

(define (same-parity first . rest)
  (cons first
        (if (even? first) 
            (filter even? rest) 
            (filter odd? rest))))

(define (filter filter-arg list)
  (cond 
    [(null? list) list]
    [(filter-arg (car list))
      (cons (car list) (filter filter-arg (cdr list)))]
    (else (filter filter-arg (cdr list)))))

;; Testing:

(same-parity 1 2 3 4 5 6 7) 
;; => (mcons 1 (mcons 3 (mcons 5 (mcons 7 '()))))

(same-parity 2 3 4 5 6 7)   
;; => (mcons 2 (mcons 4 (mcons 6 '())))