#lang planet neil/sicp

;; What I like to do now is spend the rest of the time about
;; one example. This example summarizes everything we have
;; done up to now. That is:
;; - List structure
;; - Issues of abstraction and representation
;; - Capturing commonality with higher-order procedures.

;; And it is going to introduce the major third theme in this 
;; course: meta-linguistic abstraction.
;; Which is the idea that one of the ways of tackling complexity
;; in engineering design is to build a suitable powerful 
;; language.

;; You might recall what I said is that pretty much the very
;; most important thing we are going to tell you in this course 
;; is that when you talk about a language you think about it in
;; terms of what are the:
;; - Primitives
;; - Means of combination (what are the means to build bigger 
;;                         things)
;; - Means of abstraction (how do you take these bigger things
;;                         that you build and put black boxes
;;                         around them so you can use them as 
;;                         elements and build things that are
;;                         even more complicated)

;; A friend of us, Peter Henderson, made this example:

;; Making a recursive complicated figure. Fish pattern in the
;; middle and bleed out smaller and smaller in self-similar
;; ways.

;; There is another theme which will be shown by this example,
;; that there is no real difference between procedures and data.

;; I hope by the end of this morning, if you are not already, you
;; will be completely confused about what the difference between
;; procedures and data are.

;; In any case, let's start describing Peter's language:

;; Primitives:
;; The only primitive is a picture. (A figure scaled to a 
;; rectangle that you specify.)

;; Means of combinations and operations:
;; Rotate,
;; what rotate does is rotate the image to fill in that rectangle
;; Flip,
;; flip the picture horizontally.

;; There is a means of combination called beside, this takes two
;; pictures, let's say A and B. Beside will take the two pictures
;; and take the rectangle you give it and will draw the first
;; picture A to the left and to the right the other picture B.
;; The parameter says how big one will to the other.
;; Above does the same thing but vertically instead of
;; horizontally.

(beside g (above empty g .4) .5)
(define p (beside g (rot-180 (flip g)) .5))
(define q (above p (flip p) .5))

;; In 15 seconds we come from George to this complex thing.
;; The closure principle: we do an opreation on a figure
;; and we get a new figure. Whenever I have something I can use
;; that as input to someone else, because operations are closed.

;; An element that sits under the table is a rectangle. It is 
;; specified by an origin. Some vector that says where the
;; rectangle starts, then we have the horizontal part of the 
;; rectangle. And another vector called the vertical part of
;; the rectangle. And those three pieces, where the lower vortex
;; is, how you get to the next vertex and how you can get to
;; the other vertex specify a rectangle.

;; To build rectangles I assume we have a constructor that is
;; called make-rect, and selectors called horiz, vert and origin
;; that get out the pieces of this rectangle.

;; The implementation is not our problem. Lets assume we have
;; these rectangles to work with.

;; The idea of this is that we have to take a picture and scale
;; it to a rectangle. That is the basic thing we have to arrange
;; that we can do. One way to think about it:
;; Anytime I give you a rectangle I give you a standard square
;; (0,0) (1,0) (0,1) (1,1) which has an obvious scaling 
;; transformation which stretches everything uniformly.

;; We have a line-segment like this /, and map it to a line-
;; segment like this --.

;; We can use vector algebra to do so. 
;; (x,y) |-> origin * x-horiz * y * vert

;; Let's look at that as a procedure. The thing that takes as its
;; arguments that takes a rectangle and returns a procedure
;; on points. You get a way of transforming a point (x,y) into
;; a rectangle.

(define (coord-map rect)
  (lambda (point)
    (+vect
     (+vect (scale (xcor point)
                   (horiz rect))
            (scale (ycor point)
                   (vert rect)))
     (origin rect))))

;; Any rectangle defines a coordinate map which is a procedure
;; on points.

;; I can build a primitive picture from a list of line segments:

(define (make-picture seglist)
  (lambda (rect)
    (for-each 
     (lambda (s)
       (drawline 
        ((coord-map rect) (seg-start s))
        ((coord-map rect) (seg-end s))))
     seglist)))

;; For instance:

(define r (make-rect ...)) ;; specify some vectors using 
                           ;; make-vector
(define g (make-picture ...)) ;; specify lits of line segments
                              ;; using make segment, which can be
                              ;; made out of vectors made out of
                              ;; points.
;; If I want to see an image. A picture is a procedure that takes
;; a rectangle as an argument. So if I called g with an input of
;; r, causes g to be drawn inside the rectangle r.

(g r)

;; That is how you use that.