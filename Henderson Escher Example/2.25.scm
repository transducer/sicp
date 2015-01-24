#lang racket

;; Give combinations of cars and cdrs that will pick 7 from each 
;; of the following lists:

;; (1 3 (5 7) 9)
(define a 
  (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr a)))))
(car (cdaddr a))

;; ((7))
(define b 
  (list (list 7)))
(caar b)

;; (1 (2 (3 (4 (5 (6 7))))))
(define c 
  (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr 
 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

(cadr (cadr (cadr (cadr (cadr (cadr c))))))