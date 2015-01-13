#lang scheme

;; So for we have been talking about procedures. The framework
;; we have been using is the primitive things, means of
;; combination so you can combine them, and the means of 
;; abstraction where you can take these more complicated
;; combinations and name them as simple building blocks.
;; Later we saw by using higher-order procedures we can have
;; general methods for computing things like the method of doing
;; something by fixed-points or Newton's method.
;; The crucial idea in all is this that we want to build a
;; layered system.

;; Using an abstraction boundary. So long as the functionality
;; works we do not care how it works. When we're building
;; things we divorce the task of building things with 
;; implementing the parts.

;; Now we are going to look at the same issues as above for
;; procedures for data.

;; We can use glue to put primitive data together to create
;; compound data and we will see arise a methodology which will
;; be very useful for building up data with parts of simpler
;; data. And again the key idea is to build the system in layers
;; and set up abstraction barriers that isolate the details at
;; the lower layers from the thing that is going on in the upper
;; layers. Using contracts.

;; Let's look at an example. It does arithmetic on natural
;; numbers:

;; We should ask it things as what is the sum:
;; 1/2 + 1/4 = 3/4 or 3/4 + 2/3 = 1/2

;; We have this formula:

;; n1/d1 + n/d2 = n1d2 + n2d1 / d1d2

;; n1/d1 * n2/d2 = n1n2 / d1d2

;; When we are trying to implement this we run into a problem.
;; We do not have what a rational number is.

;; We are going to solve this problem by using the design
;; strategy of wishful thinking.

;; Let's imagine that we have three procedures.

;; (make-rat n d) -> a cloud(n,d)
;; (numer cloud(n,d) -> n)
;; (denom cloud(n,d) -> d)

;; Just like assuming we have good-enough in sqrt procedure.
;; Others can choose how to implement these clouds. Once we have
;; these clouds we can do addition as follows:

(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

;; We can do multiplication in the same way.
(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

;; I assumed by wishful thinking I had some sort of data object
;; and could make one of these things (via a constructor called
;; make-rat) and select parts via selector (numer and denom) 
;; to get them.

;; Why do we want to this in the first place?
;; => Suppose you want to express things as the idea of taking
;;    two rational numbers and multiplying that by the sum
;;    of two other rational numbers.

;; E.g., (x + y) * (s + t)
;; is    (*rat (+rat x y) (+rat s t))
;; => You get an expression that mathemathically matches the 
;;    expressions. If we did not have these the sum would not
;;    work out and have to keep temporary variables.
;;    More importantly they are going to not confuse the 
;;    computer, but they are confusing our mind.

;; Question: why use the cloud to pass things in and get them
;; out again?
;; => Crucial point: I want to keep the denominator and numerator
;;    together all the time. We can call it x and just reference
;;    it that way. It's a lot like I have these instructions
;;    and I package it as a procedure and give it a name.