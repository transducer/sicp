#lang scheme

;; So far in this course we have been talking about data abstractions.
;; Systems with horizontal abstraction barriers. That separate use from the
;; way you represent it.

;;       USE
;; ==============
;; REPRESENTATION

;; This notion is a very powerful programming methodology. On the other hand
;; it is not really sufficient for very complex systems. The problem with
;; this is the representation. The problem is there can be more people
;; working on the representation. Above the abstraction barrier you don't
;; want to worry about that. You not only want a horizontal barrier, you
;; also want some kind of vertical barrier to keep representations apart.

;; Imagine you have a large company that has acquired other companies and
;; you don't want to worry about the different divisions and how the record
;; are stored. E.g., generic operators on name, salary and age.
;; You want to have vertical barriers. And you want name to be a generic
;; operator. Generic operator means that what it sort of precisely does is
;; dependent on the data it is looking at. 
;; You want to design the system so that when another system comes into the
;; company they do not have to make large changes, and the existing system
;; does not either.

;; That is the problem we will be talking about. You should think about 
;; for example distributed personal records. But here we will be talking
;; about a system that does arithmetic on complex numbers.

;; Let's take a look:

;; You can represent complex numbers either in a real part and imaginary 
;; part (rectangular) or as the distance from the origin and the angle 
;; (polar).

;; x = r cos A, r = sqrt(x^2 + y^2)
;; y = r sin A, A = arctan(y, x)

;; Adding is easier in rectancular form and multiplying is easier in polar.

;; We want arithmetic operations on complex numbers:
;; We assume we have some constructors and selectors.

(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

;; Now I have implemented the operators. Now we call on George to make a
;; complex number representation. He says, that's alright:

;;; Representing complex numbers as 
;;; pairs REAL-PART, IMAGINARY-PART

(define (make-rectangular x y)
  (cons x y))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (make-polar r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (magnitude z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (angle z)
  (atan (cdr z) (car z)))

;; We're done. What I did conceptually is no different from the rational
;; number implementation I did last time. You pick the operators and
;; provide a representation.

;; Now let's worry about Martha. See, Martha has a different idea.
;; She does not want a real part and imaginary part, she wants a pair of
;; a magnitude and a angle. She gives us this:

;;; Representing complex numbers as 
;;; pairs MAGNITUDE, ANGLE

(define (make-polar r a) (cons r a))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-rectangular x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (real-part z)
  (* (car z) (cos (cdr z))))

(define (imag-part z)
  (= (car z) (sin (cdr z))))

;; That's Martha's idea.

;; Well which idea is better? Well, if you are doing a lot of additions
;; probably George's part is better. If you are mostly doing multiplications
;; and divisions Martha's is. Or maybe, and this is the real point, you
;; can't decide.

;; What you would like then is a system that looks like this:

;        +c   -c   *c   /c
; -----------------------------------
; real-part imag-part magnitude angle
; -----------------------------------
;                 ||
; RECT            ||            POLAR
;                 ||

;; How can we do that? Suppose we could just tell by looking at a complex
;; number whether is was constructed by George or Martha. Then whenever we
;; look at a complex number we can just look at the label and know how to
;; operate on it. We want TYPED DATA. Typed data means there is some sort
;; of cloud with an ordinary data object with contents, but also a thing
;; called the type. It is signed by either George or Martha.

;; Here's a representation that supports typed data:

;;; Support mechanism for manifest types

(define (attach-type type contents) ; Take a type and attach it to a piece of
  (cons type contents))             ; content.

(define (type datum)                ; If we have a datum we can look at the
  (car datum))                      ; type which is the car.

(define (contents datum)            ; Or we can look at the cdr which is the
  (cdr datum))                      ; contents.

;;; Along with that, the way we use our data is to test of which type it is:

;;;; Type predicates

(define (rectangular? z)            ; We check whether the type is the symbol
  (eq? (type z) 'rectangular))      ; rectangular

(define (polar? z)
  (eq? (type z) 'polar))            ; or polar.

;; George and Martha made packages

;;; Rectangular package
(define (install-rectangular-package)
  (define (make-rectangular x y)
    (attach-type 'rectangular (cons x y)))
  
  (define (real-part-rectangular z)
    (car z))
  
  (define (imag-part-rectangular z)
    (cdr z))
  
  (define (magnitude-rectangular z)
    (sqrt (+ (square (car z))
             (square (cdr z)))))
  
  (define (angle-rectangular z)
    (atan (cdr z) (car z))))

;; He attaches the type which is the symbol rectangular to that pair.
;; Anything else George does is the same except that George and Martha 
;; both have the real-part and imag-part procedures. So they can't have
;; naming conflicts.

;; For martha the same thing, but with polar:

;;; Polar package

(define (install-polar-package)
  (define (make-polar r a)
    (attach-type 'polar (cons r a)))
  
  (define (real-part-polar z)
    (* (car z) (cos (cdr z))))
  
  (define (imag-part-polar z)
    (* (car z) (sin (cdr z))))
  
  (define (magnitude-polar z) (car z))
  
  (define (angle-polar z) (cdr z)))

;; Now we have the system, and we need some type of manager to look at the
;; types. How are we going to work with the typed data of Martha and George?

;; What we have is a type of generic selectors:

;;; GENERIC SELECTORS FOR COMPLEX NUMBERS

(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular
          (contents z)))
        ((polar? z)
         (real-part-polar
          (contents z)))))      ;

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular
          (contents z)))
        ((polar? z)
         (imag-part-polar
          (contents z)))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular
          (contents z)))
        ((polar? z)
         (magnitude-polar
          (contents z)))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular
          (contents z)))
        ((polar? z)
         (angle-polar
          (contents z)))))

;; Right, so there is a system that has three parts. George, and Martha and
;; there is the manager. And that's how you get generic operators
;; implemented. Let's look at a simple example. Just to pin it down.

;; Suppose we are looking at the complex number which real part 1 and
;; imaginary part 2i.

;; That number above the abstraction barrier would be defined as `rectangular
;; and the 1 and 2.

;; The manager would look at it and strip off the type and hand down to 
;; George the pair of 1 and 2. George would construct it as a pair, and 
;; before he passes it back to the manager he would attach the type
;; rectangular. So there is no confusion in the system. It does not matter
;; in the least that the pair of 1 and 2 would mean something different in 
;; Martha's world.

;; Missing stuff:
(define (square x) (* x x))


