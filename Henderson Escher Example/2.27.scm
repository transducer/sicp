#lang racket

;; Modify your reverse procedure of exercise 2.18 to produce a 
;; deep-reverse procedure that takes a list as argument and 
;; returns as its value the list with its elements reversed and 
;; with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))
;; x
;; => ((1 2) (3 4))

;; (reverse x)
;; => ((3 4) (1 2))

;;(deep-reverse x)
;; => ((4 3) (2 1))

;; Reverse from previous exercise:
(define (reverse lst)
  (define (reverse-acc lst acc)
    (if (null? lst)
        acc
        (reverse-acc (cdr lst) (cons (car lst) acc))))
  (reverse-acc lst '()))

(reverse x) ;; => '((3 4) (1 2))

;; Hint from solution:
;; Deep reverse.  Same as reverse, but when adding the car to the 
;; result, need to check if the car is a list.  If so, deep reverse 
;; it.

(define (deep-reverse lst)
  (define (reverse-acc lst acc)
    (if (null? lst)
        acc
        (reverse-acc (cdr lst) 
                     (cons 
                      (if (pair? (car lst))
                          (deep-reverse (car lst)) (car lst))
                      acc))))
  (reverse-acc lst '()))

(deep-reverse x) ;; => '((4 3) (2 1))

;; Testing doubly nested list:
(define y (list (list (list 1 2) (list 3 4)) (list 5 6)))

(reverse y) ;; => '((5 6) ((1 2) (3 4)))
(deep-reverse y) ;; => '((6 5) ((4 3) (2 1)))

;; Shortest from solution:
;; (Note: uses Schlemiel the Painter's algorithm since with
;; append we have to traverse the whole list each time just to
;; put one element last.)
(define (deep-reverse-from-solution x) 
   (if (pair? x) 
       (append (deep-reverse-from-solution (cdr x))  
               (list (deep-reverse-from-solution (car x)))) 
       x))

(deep-reverse-from-solution y) ;; => '((6 5) ((4 3) (2 1)))
