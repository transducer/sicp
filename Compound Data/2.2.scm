#lang planet neil/sicp

;; Consider the problem of representing line segments in a plane.
;; Each segment is represented as a pair of points: a starting 
;; point and an ending point. Define a constructor make-segment 
;; and selectors start-segment and end-segment that define the 
;; representation of segments in terms of points. 

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; Furthermore, a point can be represented as a pair of numbers: 
;; the x coordinate and the y coordinate. Accordingly, specify a 
;; constructor make-point and selectors x-point and y-point that
;; define this representation. 

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; Finally, using your selectors and constructors, define a 
;; procedure midpoint-segment that takes a line segment as 
;; argument and returns its midpoint (the point 
;; whose coordinates are the average of the coordinates of the 
;; endpoints). 

(define (midpoint-segment segment)
  (define (average x y) (/ (+ x y) 2))
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

;; To try your procedures, you'll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Testing:
(define point-a (make-point 1 1))
(define point-b (make-point 3 3))
(define test-segment (make-segment point-a point-b))

(print-point (midpoint-segment test-segment)) ;; => (2,2)