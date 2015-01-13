#lang scheme

;; We have looked at a simple system of doing arithmetic
;; on natural numbers as an example of doing data abstraction
;; on large systems for controlling complexity.

;; Like procedure definition the real power of these things show
;; up not when you these things by themselves, but when you use
;; these as building blocks for making more complicated things.

;; Always ask about building blocks: what can I build with it?

;; Another example:
;; Representing vectors in the plane

(define (make-vector x y) (cons x y)) ;; Constructor
(define (xcor p) (car p))             ;; Selector
(define (ycor p) (cdr p))             ;; Selector

;; We might use them to build something.

;; P(1,2)    Q(2,3), and we want to talk about the line segment.

;; We can represent line segments the same way:
(define (make-seg p q) (cons p q))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

;; Now we can have some operations on them. What is the midpoint:

(define (midpoint s)
  (let ((a (seg-seg s))
        (b (seg-end s)))
    (make-vector
     (average (xcor a) (xcor b))
     (average (ycor a) (ycor b)))))

;; And the length of a segment:

(define (length s)
  (let 
      ((dx (- (xcor (seg-end s))
              (xcor (seg-start s))))
       (dy (- (ycor (seg-end s))
              (ycor (seg-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))

;; And again: what we build is a layered system:

;;           Segments
;; ==============================
;; make-seg / seg-start / seg-end
;; ==============================
;;           Vectors
;; ==============================
;; make-vector / xcor / ycor
;; ==============================
;;       Pairs and numbers

;; To represent the segment we have a pair:
;; a vector P which consists of a pair of numbers, and a vector
;; Q which also consists of two numbers.

;; Let's talk about closures. By closure I mean that:
;; the means of combinations in your system are such that 
;; when you put things together using them, you can then put
;; those together with the same means of combination.
;; I can not only have a pair of numbers, but I can have a pair
;; of pairs.
;; To get the quality of a meas of combination ask: 
;; are these things closed by a means of combination.
;; (case in point: Fortran cannot make array of arrays).
;; If all I can do is a pair of numbers I cannot build very
;; much abstraction at all.

;; We'll here more about closures later on.

;; We can perfectly well implement the structures differently:
(define (length s)
  (let ((dx (- (car (car s)) (car (cdr s))))
        (dy (- (cdr (ar s)) (cdr (cdr s)))))))

;; This is much harder to read, but worse than that. We have not
;; implemented length. Then someone says I want to write points
;; with the x-coordinate first. The next day someone comes back
;; and says we have to write the end-point first.
;; Then the next day: "What I really meant is from left to right"
;; and then you just want to punch 'em in the face.

;; We've named the system. Choices behind the representation
;; are localized right there. That is the real power of this
;; system. We are explicit about hem so we have control over
;; them.

;; Question: in three dimensional space we have three
;; coordinates. What would happen? The point is that once you 
;; have two things you can make as many things as you want.
;; You can make pairs of one first pair and another element
;; that consists of two pair. 
;; There are conventions on how to do things like this and we'll
;; talk about that later.
;; It all comes from being able to make pairs of pairs. If all I
;; could do was make pairs of numbers I'd be stuck.

