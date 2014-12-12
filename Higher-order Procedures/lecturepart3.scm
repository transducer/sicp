#lang scheme

;; Helper functions:
(define (square x) (* x x))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter u v)
    (if (close-enuf? u v)
        new
         (iter v (f v))))
    (iter start (f start)))

;; So now we have seen how to use higher-order procedures.
;; That is, use procedures that take procedural arguments
;; and produce procedural values to help us clarify and abstract
;; some otherwise complicated processes. 
;; I suppose what I like to do now is to have a bit of fun with 
;; that.

;; Let's play with the square roots a bit more:

;; Newton's method's to find the roots (that's the zeroes)
;; of functions.

;; To find a y such that:
;; f(y) = 0
;; Start with a guess, y0
;;                   f(yn)
;;   yn+1 = yn - ------------
;;                df/dy |
;;                      |y=yn

;; Very strange notation. I must say "ugg". The derivative of
;; f with respect to y is a function. I have a bit of unhappiness
;; with that, but that's alright.
;; Turns out the programming language world's notation is 
;; much clearer.

;; It sometimes converges and when it does it does so very fast.
;; Sometimes it does not and then we have to do something else.

;; Let's do some wishful thinking, apply Newton's method while
;; we do not know how to do it yet.

(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          ;; Indeed, if I had a value of y where for this was
          ;; zero then that would be the square root of x.
          1))
;; Start at value of 1 again, completely arbitrary property of
;; square roots that I can do that.

;; Now try to find a fixed point of that procedure which involves
;; some complicated expressions in terms of other complicated
;; things. But I am just trying to find a fixed point of that.

;; INTERMEZZO:
;; Functions are mathemathical things, mapping sets of values
;; to others. Procedures compute functions.

;; The value of yn so that I put a value in I get the same 
;; value as yn+1 up to some degree of accuracy.

(define (newton f guess)
  (define df (deriv f))
  (fixed-point 
   (lambda (x) (- x (/ (f x) (df x))))
   guess))

;; Wishful thinking is essential to good engineering.
;; And certainly essential to good computer science.

(define deriv
  ;; A derivative is a procedure that takes a procedure that
  ;; computes a function as its argument
  (lambda (f)
    ;; And it produces a procedure that computes a function which
    ;; needs one argument x so that
    ;; "df/dx = (f(x + dx) - (f(x)) / dx", 
    ;; for some small delta x.
    (/ (- (f (+ x dx)
             (f x))
          dx))))

;; I suppose I haven't told you what dx is. Somewhere in the
;; world I have to write down:

(define dx .00001) ;; I am not interested.

;; What we have build is a very powerful engine that can be used
;; to compute nice things like this.

;; I want to end this with an idea of Chris Strachey,
;; one of the grandfathers of computer science. A logician
;; and one of the inventors of denotational semantics.
;; He was a great advocate of making procedures or functions
;; first-class citizens in a programming language.

;;     The rights and priviliges of first-class citizens:
;;
;; To be named by variables.
;; To be passed as arguments to procedures.
;; To be returned as values of procedures.
;; To be incorporated into data structures.

;; This way you can make any powerful abstraction you like.
;; This way you can encode general methods like Newton's method
;; in a very clear way.
;; We will soon see the latter right to incorporate functions in
;; data.

