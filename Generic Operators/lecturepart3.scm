#lang scheme

;; We just looked at data-directed programming as a way of implementing
;; a system that does arithmetic on complex numbers. It had some operations
;; like +c, -c, *c and /c and that was on top of two different
;; representations (rectangular and polar) and we showed that others were
;; easy to add.

;; But, that does not show the power of this methodology. The power of the 
;; methodology becomes apparent when it is embedded in a more complex
;; system. That's what we are going to do now.

;; Let's assume we have a generic arithmetic system. At the top level 
;; someone can say ADD, SUB, MUL, or DIV two things. Underneath there is an
;; abstraction barrier and underneath is a complex arithmetic package, or a
;; rational number package or ordinary LISP numbers.

;; ADD          SUB          MUL          DIV
;; ------------------------------------------
;;  RATIONAL    |    COMPLEX    |   ORDINARY
;;   +rat       |    +c   -c    |      +
;;   *rat       |    *c   /c    |      -
;;   /rat       |               |      *
;;   -rat       |---------------|      /
;;    .         | RECT  | POLAR |
;;    .         |       |       |

;; Let's look at how we have to change the packages.

;;; Rational number arithmetic

(define (+rat x y) ; This is actually same code as last time.
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer x)))
            (* (denom x) (denom y))))

(define (-rat x y) ...)
(define (*rat x y) ...)
(define (/rat x y) ...)

;; @ 4B: Generic Operators 48.11