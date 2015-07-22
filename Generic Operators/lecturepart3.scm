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

;; We're ignoring the gcd-problem. As implementers of rational numbers how
;; do we install it in the generic arithmetic system? Well that is easy:

;;; installing rational numbers in the
;;; generic arithmetic system

(define (make-rat x y)
  (attach-type 'rational (cons x y)))

(put 'rational 'add +rat)
(put 'rational 'sub -rat)
(put 'rational 'mul *rat)
(put 'rational 'div /rat)

;; To make a rational number you build a pair of the numerator and the 
;; denominator, and here we are not only building the pair but also signing
;; it to the type rational. Making it a typed data object is the only thing
;; we have to do different. And we add the procedures to the symbols in 
;; the table.
;; That is how to make it fit into the generic arithmetic system.

;; We want to have the generic operators like ADD, SUB DIV and MUL. To do
;; this we do the following:

(define (add x y)
  (operate-2 'add x y))

(define (operate-2 op arg1 arg2)      ; to operate with some operator on
  (if                                 ; two arguments
   (eq? (type arg1) (type arg2))      ; we are first checking if the two 
   (let ((proc (get (type arg1) op))) ; arguments have the same type.
     (if (not (null? proc))           ; If there not we will complain below.
         (proc (contents arg1)        ; Else we'll look in the table for a
               (contents arg2))       ; procedure, and if so we'll apply it
         (error                       ; to the contents of the arguments.
          "Op undefined on type")))   ; Or if not found we give an error.
   (error "Args not same type")))

;; How to we embed our complex number package in our generic system?
;; Almost the same:

;;; installing complex numbers

(define (make-complex z)    ; we define a procedure make-complex
  (attach-type 'complex z)) ; that take whatever George or Martha hand to us
                            ; and add the type complex.
(define (+complex z1 z2)    ; Then we say, to add complex numbers we use 
  (make-complex (+c z1 z2))); our internal procedure +c and attach a type.

(put 'complex 'add +complex); Add the procedure to the table.

;; Similarly for -complex *complex /complex

;; To communicate with the outside world we have +complex and -complex. 
;; The only difference with +c and -c (internal opreators) is that they are 
;; typed.

;; How do we install ordinary numbers? Same way:

;;; installing ordinary numbers

(define (make-number n)    ; make-number takes a number and attached a type
  (attach-type 'number n)) ; which is the symbol number.

(define (+number y)        ; we built the procedure plus number
  (make-number (+ x y)))   ; which simply adds with the ordinary addition
                           ; and make a type of it
(put 'number 'add +number) ; and add it to the symbol table

;; similarly for -number *number /number

;; Let's look at an example to make it clear:
;; Suppose I'm going to perform the operation which looks like multiplying
;; two complex numbers, e.g.,

; (3 + 4i) * (2 + 6i) And we add it to our generic operator MUL.

;; Let's say it was of the type of George's rectangular type of type complex

; | 'complex | - | > | 'rect | - | > | 3 | 4 |

;; That is what this object would look like at the generic level. MUL would
;; come along and see that its type is complex. It goes to the operate-2 and
;; says "Oh, what I want to do is what is in the table which is the
;; procedure *complex, on the 'complex thing with the type stripped off and
;; send that down to the complex world. The complex world looks down at
;; its operators and sees it has to apply *c. *c might at some point say
;; "Oh, I want to look at the magnitude of this object that it's got" and
;; will then say "Oh, it is rectangular" and it will then strip off the next
;; type and will hand it to George to take the magnitude of.

;; So there are these chains of types, and the amount of types are the
;; amount of levels you will go up in the table. What a type tels you is
;; that everytime you have a vertical barrier in the table, the type is
;; telling you where to go. And everytime you go back up the stuff at
;; the bottom sticks the type back on. So that's the general structure of
;; the system.

;; Okay, now that we've got this. Let's go and make this thing even more
;; complex. Let's talk about adding to this system not only numbers, but
;; also polynomials. It might be nice to do arithmetic on polynomials like

;; x^15 + 2x^7 + 5

;; and if we have two of those we can add them, subtract them or multiply 
;; them. Let's not worry about dividing. So what do we have to do?

;; Let's think about how we might represent a polynomial. It's going to be
;; a typed data object:

;; First it says the name, then the variables, and some info on the terms.

(polynomial x <term-list>)

;; e.g.,

((15 1) (7 2) (0 5))

;; Polynomial starts with order 15 and coefficient 1, et cetera.
;; There are a lot of tradeoffs, this is a fairly standard representation
;; that's useful in a lot of contexts.

;; So how do we implement our polynomial arithmetic? Let's start out:

;;; Installing polynomials

; First have a way to make polynomials 
(define (make-polynomial var term-list) ; And we are making one out of a
  (attach-type 'polynomial              ; variable and a term-list and we
               (cons var terms-list)))  ; package them together some way
                                        ; and attach the type.
(define (poly p1 p2)                    ; How to add two polynomials p1 and
  (if (same-var? (var p1) (var p2))     ; p2? Let's say we only add things 
      (make-polynomial                  ; of the same variable. If they are
       (var p1)                         ; not the same we give an error.
       (+terms (term-list p1)           ; If they are, we make a polynomial 
               (term-list p2)))         ; who's variable is whatever that
      (error "Polys not in same var"))) ; variable is and who's term-list is
                                        ; something we call +terms, which
(put 'polynomial 'add 'poly)            ; will add the two term-lists. And
                                        ; we add it to the symbol table.

;; We haven't really done anything. We have pushed all the work to +terms.
;; Let's look at that. Here's an overview of how we might add two 
;; term-lists:

(define (+terms L1 L2)                 ; L1 and L2 are two term lists
  (cond ((empty-termlist? L1) L2)      ; consisting of a bunch of terms.
        ((empty-termlist? L2) L1)      ; Then comes a case analysis.
        (else                          ; We are recursively working through
         (let ((t1 (first-term L1))    ; them till either L1 or L2 is empty.
               (t2 (first-term L2)))   ; Our answer will be the other one.
           (cond                       ; Otherwise there are three cases.
             ((> (order t1) (order t2)); If the order is greater our answer
              (adjoin-term             ; will start with the term of t1
               t1                      ; because it won't combine with lower
               (+terms (rest-terms L1) L2))) ; order terms, so we will add
             ((< (order t1) (order t2))      ; them of the rest in L1 and L2
              (adjoin-term                   ; Then we will adjoin the 
               t2                            ; higher order term
               (+terms L1 (rest-terms L2)))) ; Other case same way, but vice
             (else                           ; versa. If we have to add them
              (adjoin-term                   ; we make a term and we will
               (make-term (order t1)         ; add the coefficients.
                          (ADD (coeff t1)    ; This is a big recursive 
                               (coeff t2)))  ; working down of terms. But
               (+terms (rest-terms L1)       ; really, there is only one
                       (rest-terms L2))))))))); interesting idea. That's ADD.
         
;; And the reason that is interesting is because something really wonderful
;; just happened. We reduced adding polynomials not to plus, but to the
;; generic plus. In other words by implementing it that way, we do not only
;; have a system where we can have rational numbers or complex numbers or
;; ordinary numbers, but we also have polynomials who's coefficients can be
;; anything the system can add. So these can be polynomials who's 
;; coefficients can be rational or complex, who can be either rectangular
;; or polar, or ordinary numbers.

;; So that I mean precisely is that our system right now automatically can 
;; add things like this:

; 2/3x^2 + 5/16x + 11/4

; (3 + 2i)x^5 + (4 + 7i)

;; Why can it automatically handle those things? That's profoundly because
;; we reduced polynomials to adding their coefficients, and adding their
;; coefficients was done by the generic add operator who says "I don't care
;; what your types are as long as I know how to add you". So for free we get
;; the ability to handle that.

;; Well, it is even better than that, cause remember one of the things we did
;; is that we put into the symbol table +poly. That means polynomials 
;; themselves are things that can be added. E.g.,

; (x^2 + i)y^2 + (x^3 - 2x)y + (x^4 - 7)

;; This is a polynomial in y who's coefficients are polynomials in x.

;; Simply by saying polynomials are things that can be added, not only can 
;; we deal with rational, ordinary or complex numbers, but we can also deal
;; with polynomials who's coefficient are rational, ordinary or complex, or
;; are polynomials who's coefficients are rational, ordinary, complex
;; (rectangular or polar), or are polynomials who's coefficients are ...

;; And so on. So this is sort of an infinite tower of types that we have
;; built up. And it is all from writing ADD instead of + and the polynomial
;; things.

;; The polynomials are a constructor for types. Namely, you give it a type
;; like integer and it returns for you polynomials in x who's coefficients
;; are integers. And the important thing about that is that the operations
;; on polynomials reduce to operations on the coefficients. And there are
;; a lot of things like that. For example in rational numbers we have
;; the idea of an integer over an integer, but there's the general notion of
;; a rational object. So we might think about (3x + 7) / (x^2 + 1) and that
;; is a sort of general rational object who's numerator and denominator are 
;; polynomials. And to add two of them we add two formulas. How can we 
;; install that in our system?

;; All we have to change to our rational number arithmetic package is
;; replace the particular plusses and stars to ADD and MULL. E.g.,

(define (+rat x y)
  (make-rat
   (ADD (MULL (numer x) (denom y))
        (MUL (denom x) (numer y)))
   (MUL (denom x) (denom y))))

;; Then suddenly are entire system can start talking about objects who look
;; like this:

;      x + 3 + 2i / 5 - 6i
; ----------------------------
; x^2 + (1 + 2i / 3 + 7i)x + 1

;; A rational object who's numerator is polynomials in x who's coeffecients
;; are rational objects constructed out of complex numbers. And a lot of 
;; others like two-by-two matrices who's elements are rational objects.

;; All that comes for free. What's really going on is that getting rid of
;; this manager who's poking his nose into's what's everybody's business has
;; made us built a system that has DECENTRALIZED CONTROL. So when you come
;; into add, no one is poking around like "Gee are you in the official list
;; of people who can be added", but rather "go add yourself like your parts
;; like to be added" and then the result of that is we can have a very 
;; complex hierarchy and a lot of things can get done and will be done in
;; the right place automatically.

;; QUESTIONS:

;; You get this for free? You've lost the cleanness of the break what is on
;; top and what is underneath. You are defining lower level procedures in
;; terms of things that are above the line. Isn't that dangerous?
;; => All this doing is recursion. It's saying that the way you add these
;;    guys is to add these guys. So I don't think it is any less structured
;;    or less clean.

;; But if you change the MUL or ADD operator you can have tremendous 
;; consequences underneath that you do not know the extend of. 
;; => That's right, but it depends on what you mean. See it goes both ways.
;;    I ignored greatest common divisor, but if I suddenly decided that +rat
;;    should do a GCD computation and install it, then that comes available
;;    to all the other types. So it depends on what you mean with the 
;;    coherence of your system. You sure might need a special different one.
;;    But most times here you don't that the beauty of this system.

;; The types get more and more complex when they're running?
;; => Yes. Recursion. There is no finite list of things they might look like
;;    so if you want to specify the system you have to do it in another way
;;    than a finite list: with a recursive structure.

;; How do you initially setup the data structure so that everything goes
;; to the right spot if I don't have the packages?
;; => That's the wonderful thing you just say MUL.

;; And it knows that I have complex numbers?
;; => You will have a constructor to make complex numbers.

;; I have to make them?
;; => What you will probably have as a user is some little thing in a reader
;;    loop which would give you some plausible way to type in a complex 
;;    number in whichever format you like. Or someone is just handing them
;;    to you.

;; So I have to construct them?
;; => Yes if you make them yourselves. But what you do know is that you can
;;    just say MUL when you do have a complex number and it will multiply.

;; If I want to change the operation of complex, how much real code would
;; I have to get around with or change?
;; => You only have to change one thing at one place. It will propagate.
;;    That's the power of this particular one.

;; Is the real world just as smooth as the example above? I can imagine that
;; in the real world you have to keep bashing and don't even know all the
;; moving parts till you have a proper working version where everything is 
;; falling out for free.
;; => Well, that certainly is a very intelligent question. One part is that
;;    this is a methodology that people have discovered coming from symbolic
;;    algebra. Cause there are lot of complications which allow you to
;;    implement these things before you decide what types of operations you
;;    allow. And in some sense this is an answer that people have discovered
;;    by wading through this stuff. In another sense it is a very contrived
;;    example.
;;    Let me show you.
;;    Notice that is breaks down when I ask it to do something like 3 + 7/2.
;;    Because operate-2 will say well this is of type number and that is of
;;    type rational, I don't know how to add them. At least you want the 
;;    system to do something like 3/1 + 7/2, have it changed to a rational
;;    number so it can use the rational package.

;; That's the thing I didn't talk about in the lecture but is in the book:
;; a thing called COERCION. After having so carefully setup all these types
;; you want to also put in knowledge on how to view an ordinary number as 
;; a kind of rational, or complex. That's where the complexity in the system
;; really starts happening. 
;; There are terrible examples like 3+7i + 5/7, then somebody got to know
;; that I need to convert this to complex numbers who's parts might be
;; rationals. And who worries about that? Is that complex, plus or
;; rational? That's where the real complexity comes in. And this in fact is
;; pretty wel sorted out. And a lot of this message-passing stuff was 
;; motivated by problems like this. And when you really push it... somehow
;; the algebraic manipulation problem is so complex - that the people who
;; are always at the edge of it are exactly in the state the last question
;; asker described. Wading, mucking, et cetera.

;; It certainly seems true that you can alter lower level things, but you
;; are freezing higher level operations.
;; => That is an extremely good question. See, what I have to do is... If I
;;    decide there is a new general operation like "equality-test". Then all
;;    of these people have to decide whether or not they would like to have
;;    an equality test by looking in the table. There are ways to
;;    decentralize it even more. That's where I hinted at last time, you
;;    might not only have the type as a symbol, but you might store in
;;    each object the operations that it knows about. So you might have
;;    things like greatest-common-divisor. So it might be a very fragmented
;;    system and depending on where you want your flexibility you have a 
;;    whole spectrum of places where you can built that in.
;;    But you are pointing at the place where this starts being weak. That
;;    there needs to be an agreement on top, or that you will have a table
;;    that will be very sparse. But there are a lot of ways to play that
;;    game.
