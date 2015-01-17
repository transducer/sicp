#lang planet neil/sicp

;; Define a procedure last-pair that returns the list that 
;; contains only the last element of a given (nonempty) list:

(define (last-pair input-list)
  (if (null? (cdr input-list))
      input-list
      (last-pair (cdr input-list))))

(last-pair (list 23 72 149 34)) ;; => (34) = (mcons 34 '()) ?