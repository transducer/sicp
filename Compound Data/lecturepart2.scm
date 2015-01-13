#lang scheme

;; We have been working on rational arithmetic problem using
;; selectors and constructors and implemented addition and
;; multiplication.

;; There is a primitive operator in lisp called cons. 

(cons x y) ;; => Constructs a pair whose first part is x and
           ;;    whose second part is y.
(car p)    ;; => Selects the first part of the pair p.
(cdr p)    ;; => Selects the second part of the pair p.

;; Box and pointer notation 2 <- [  |  ] -> 3

;; Is unobvious why this is called list structure.
;; About car, cdr and cons I know:

;; For any x and y
(car (cons x y)) is x
(cdr (cons x y)) is y

;; Given this it is pretty clear how someone else can construct
;; rational numbers.

(define (make-rat n d)
  (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

;; Now we are in business. A complete implementation of rational
;; numbers. Suppose I want to add one half to one fourth:

(define a (make-rat 1 2))
(define b (make-rat 1 4))
(define ans (+rat a b))
(numer ans) ;; => 6
(denom ans) ;; => 8

;; Why is it not 3/4? A bug.
;; What is wrong with our implementation with +rat?

;; Well the answer is that one way to look at is that nothing is
;; wrong. Not our problem.
;; Or should we reduce the stuff to lowest term?

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

;; This way it will be reduced to lowest term automatically.
;; This is a complete system for rational arithmetic.

;; What have we done?
;; We had a thing +rat and *rat and implemented them.
;; We might need -rat, printing, et cetera. And we implemented
;; these in things of pairs with cons, car and cdr.

;; We set up an abstraction barrier

;;            +rat  *rat  -rat            USE
;; ==========================================
;; ABSTRACTION make-rat / numer / denom LAYER
;; ==========================================
;;                     pairs   REPRESENTATION

;; Isolating the use of data objects from the representation.
;; This is called data abstraction.

;; Defining +rat without data abstraction

(define (+rat x y)
  (cons (+ (* (car x) (cdr y))
           (* (car y) (cdr x)))
        (+ (cdr x) (cdr y))))

;; What is the point of using this data abstraction?

;; One of the most important concepts in programming is naming
;; and is the same as in sorcery:
;; If you have the name of the spirit you have control over it. 

;; Nowhere in the system above of +rat can we point at something
;; the conceptual entity of a rational number.

;; It depends on the problem domain which implementation you use.
;; The real issue is that you might not be able to decide.
;; As system designers you are forced with the necessity in a
;; way how to do thing. Never make up your mind about anything
;; until you have to. There is a thin line between this and
;; procrastination. If you like to make progress but also never
;; be bound by the consequences of your decisions. A data
;; abstraction is one way of doing this.
;; Giving a name to the decision of how we are going to do it
;; and that continuing as if we have made a decision.

;; That is a very powerful design technique. We are going to see
;; this idea again and again.

;; Question: what does this do to the axiom do all your design
;; before you code? 
;; Answer: well, that is someone's axiom. And I bet that is
;; someones axiom that has not implemented very large computer
;; systems.
;; Computer science's good part is a big like magic. There's a
;; bad part of computer science which is a lot like religion.
;; The real power is that you can pretend that you have made the
;; decision and then later on figure out which decision you 
;; ought to have made. When you do that you have the best of both
;; worlds.
;; Question: difference between let and define.
;; Answer: let is used for local names.
(define a 5)
(let ((z 10))
  (+ z z)) ;; => 20

z ;; => unbound
;;  Changing a to z will keep (+ a a) 20.
;; You are typing in an environment where you define things.
;; Let is for a local context for a definition.
;; There are some other differences but this is the main one.