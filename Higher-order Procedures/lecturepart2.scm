#lang scheme

;; Helper functions:
(define (average x y) (/ (+ x y) 2))

;; 23:06 Like I said 'computers to make people happy, not
;; people to make computers happy'. The main reason we are
;; using all this abstraction stuff is so that programs can 
;; be more easily written and more easily read.

;; Hero of Alexandria's method of square roots was in any case
;; a program that is kinda complicated. 
;; It is not clear what it is computing...

(define (sqrt x)
  (define tolerance 0.00001)
  (define (good-enuf? y)
    (< (abs (- (* y y) x)) tolerance))
  (define (improve y)
    (average (/ x y) y))
  (define (try y)
    (if (good-enuf? y)
        y
        (try (improve y))))
  (try 1))

(sqrt 2) ;; => 1+168/408

;; There is some loop here inside try, and this loop does
;; something of trying the improvement of y.
;; Uhh, there is something called improve that does some 
;; averaging and quotienting and stuff.

;; But what is the real idea? Can we make it clear what the idea
;; is?
;; I think we can. Use abstraction.

;; What we have mathematically is a method:

;;    f    y + x/y
;; y|----> -------
;;            2
;; f(sqrt(x)) = sqrt(x)

;; Look for a Fixed-Point of the function f.
;; A Fixed-Point of a function is something that you can put
;; in a function and get the same value out.

;; Some functions have the property that you can find their
;; fixed point by iterating the function. 
;; (E.g., keep pressing cosine on the calculator. 
;;  And the method of Hero of Alexandria for calculating sqrt.)

(define (sqrt2 x)
  (fixed-point
   (lambda (y) (average (/ x y) y))
   1))

;; What we see here is that I am just trying to write it out
;; using wishful thinking. I don't know how I am going to make
;; fixed point happen. We worry about that later.

;; But if somehow I had a way of finding the fixed point of
;; the function computed by this procedure than that would be
;; the square root that I am looking for.

;; How are we going to come up with fixed-point?

(define (fixed-point f start)
  ;; I need to find out when I am done. One way of knowing
  ;; that I am done is when the old value and the new value
  ;; are close enough so I cannot distinguish them anymore.
  (define tolerance 0.00001)
  (define (close-enuf? old new)
    (< (abs (- old new)) tolerance))
  ;; Loop inside of here that will push the button on the
  ;; calculator hoping that it will eventually converge.
  ;; Internal loops are written inside procedures.
  (define (iter old new)
    (if (close-enuf? old new)
        ;; I take the new one if close enough, otherwise
        ;; we use the new value for old, and call f on new.
        new
         (iter new (f new))))
    ;; There are two registers on the calculator that we keep
    ;; pushing.
    ;; Now start this thing up by giving two values.
    (iter start (f start)))
  
;; But let's see, I haven't told you enough... 
;; It is actually easier than this.
;; There is more structure to this problem than I already told u.
;; I mean, why should this work? Why should it converge?

;; It isn't so obvious. Surely there are other procedures who's
;; fixed point would also be the square root.

;; The obvious one would be:

;;    g     x
;; y|----> ---
;;          y

;; Why don't I use it? Because you never get close to the square
;; root. It oscillates. It is a signal processing circuit.
;; I don't want to damp out the oscillations.
;; I can do that: by taking the last two values of something 
;; that oscillates you gain something in between. 

(define (sqrt3 x)
  (fixed-point ;; of a procedure resulting from average damping.
   (average-damp (lambda (y) (/ x y)))
   1))

(define average-damp 
  (lambda (f)
    (lambda (x) (average (f x) x))))

;; This is a very special thing. For the first time you are 
;; seeing a procedure that produces a procedure as its value.
;; The average-damp procedure takes a procedure f and does
;; something to it to produce a new procedure of one argument
;; x, which averages f applied to x and x itself.

;; This is a clearer way of writing it down since it provides
;; a name and really explains what Hero of Alexandria was doing.

;; QUESTIONS:
;; Why does average-damp not need a formal parameter?

;; I could have written another way: (define (average-damp f).
;; is a different way of writing:
;; (define average-damp (lambda (f).
;; You have to get used to lambda notation.

;; How to write average-damp without lambda?

(define (average-damp2 f)
  (define (foo x)
    (average (f x) x))
  foo)

;; A lambda is a way of writing an anonymous procedure.
;; You do not have to give it a name, like foo above.
;; A cuter way of not defining the anonymous procedure we will
;; talk about later.









  