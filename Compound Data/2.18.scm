#lang planet neil/sicp

;; Define a procedure reverse that takes a list as argument and 
;; returns a list of the same elements in reverse order:

(define (reverse list)
  (if (null? list) 
      list
      (cons (reverse (cdr list)) (car list))))

;; => (mcons (mcons (mcons (mcons (mcons '() 25) 16) 9) 4) 1).

;; http://stackoverflow.com/questions/28005166/
;; what-is-the-difference-between-mcons-mcons-25-16-and-
;; mcons-25-mcons