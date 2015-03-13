#lang scheme

;; We can represent a set as a list of distinct elements, and we can
;; represent the set of all subsets of the set as a list of lists. 
;; For example, if the set is (1 2 3), then the set of all subsets is 
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). 
;;
;; Complete the following definition of a procedure that generates the 
;; set of subsets of a set and give a clear explanation of why it works:

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map <??> rest)))))

(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3)) 
;; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(subsets (list 1 2 3 4))
;; => (()
;;    (4)
;;    (3)
;;    (3 4)
;;    (2)
;;    (2 4)
;;    (2 3)
;;    (2 3 4)
;;    (1)
;;    (1 4)
;;    (1 3)
;;    (1 3 4)
;;    (1 2)
;;    (1 2 4)
;;    (1 2 3)
;;    (1 2 3 4))

;; The set of all subsets of a given set is the union of:
;; - the set of all subsets excluding the first number.
;; - the set of all subsets excluding the first number, 
;;   with the first number re-inserted into each subset. 
