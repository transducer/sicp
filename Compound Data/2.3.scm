#lang planet neil/sicp

;; Implement a representation for rectangles in a plane. (Hint: 
;; You may want to make use of exercise 2.2.) 

;; "Import" exercise 2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; Implement rectangle:
(define (make-rectangle lower-left upper-right)
  (cons lower-left upper-right))

(define (lower-left rectangle) (car rectangle))
(define (upper-right rectangle) (cdr rectangle))

;; In terms of your constructors and selectors, create procedures
;; that compute the perimeter and the area of a given rectangle. 

;; (x1, y2)--(x2, y2)    
;; |                |
;; (x1, y1)--(x2, y1)

(define (perimeter rectangle)
  (let ((x1 (x-point (lower-left rectangle)))
        (x2 (x-point (upper-right rectangle)))
        (y1 (y-point (lower-left rectangle)))
        (y2 (y-point (upper-right rectangle))))
    (* 2 (+ (- x2 x1) (- y2 y1)))))

(define (area rectangle)
  (let ((x1 (x-point (lower-left rectangle)))
        (x2 (x-point (upper-right rectangle)))
        (y1 (y-point (lower-left rectangle)))
        (y2 (y-point (upper-right rectangle))))
    (* (- x2 x1) (- y2 y1))))

;; Now implement a different representation for rectangles.

(define (make-rectangle-by-points a b c d)
  (let ((lower-left a)
        (upper-right c))
    (cons lower-left upper-right)))

;; Can you design your system with suitable abstraction barriers, 
;; so that the same perimeter and area procedures will work using
;; either representation? 

;; If we use representation of lower-left and upper-right.

(area (make-rectangle (make-point 1 1) (make-point 3 3)))
(perimeter (make-rectangle (make-point 1 1) (make-point 3 3)))

;; Same for make-rectangle-by-points. A bit lame though.

;; Solution has other alternatives by using sides.
