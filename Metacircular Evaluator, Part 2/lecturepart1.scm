#lang scheme

; What we did so far was a lot of fun, but was it also useful for something? I suppose the
; answer is going to be "Yes". These circular interpreters are a valuable to thing to 
; play with. Some years I spent 50% of my time trying various design alternatives by
; experimenting with them with metacircular interpreters -- metacircular interpreters
; like the sort we just saw. 

; They are called metacircular because they are defined in terms of themselves in such a 
; way that the language they interpret contains itself. Such interpreters are a convenient 
; medium for exploring language issues. If you want to try adding a new feature, it's sort 
; of a snap, it's easy, you just do it and see what happens.

; See what difference it makes if you make slight changes to for example the binding
; strategy. If you don't like it, you throw it away. In fact these metacircular interpreters
; are an excellent medium for people exchanging ideas about language design, because they
; are pretty easy to understand, short and compact, and simple.

; I want to show you some of that technology. See, because it is the essential simple
; technology for getting started in designing your own languages for particular purposes.

; Let's start by adding a very simple feature to a Lisp.

; Now, there is one thing I want to tell you about features, before I start. There are many
; languages that have made a mess of themselves by adding huge numbers of features. 
; Computer scientists have a joke about bugs that transform into features all the time. 
; But I like to think of it is that many systems suffer from what's called 
; CREEPING FEATURISM. Bob adds his favorite features, Glenn his, Maria hers, et cetera.

; In some cases it is reasonable to have a lot of features (e.g., editors), because there
; are a lot things you want to be able to do and many of them are arbitrary. But in
; computer languages, I think it's a disaster to have too much stuff in them. 

; The other alternative you get into is something called FEEPING CREATURISM, which is where
; you have a box which has a display, a fancy display, and a mouse, and there is all sorts
; of complexity associated with all this fancy IO. And your computer language becomes a
; dismal, little, tiny thing that barely works because of all the swapping, and disk
; twitching, and so on, caused by your Windows system. And every time you go near the 
; computer, the mouse process wakes up and says: "Gee, do you have something for me to do?",
; and then it goes back to sleep. And if you accidentally push your mouse with your elbow,
; a big puff of smoke comes out of your computer and things like that.

; So two ways to disastrously destroy a system by adding features. Let's try right now to
; add a little, simple feature.

; This in fact a good one, and in fact real Lisps have it. As you've seen there are
; procedures like plus and times that take any number of arguments. So we can write things
; like the sum of the product of a and and x, and the product of b and x and c:

(+ (* a x x)
   (* b x)
   c)

; And as you can see here, addition takes three arguments or two arguments, multiplication
; takes two arguments or three arguments, all are taking numbers of arguments which are 
; going to be treated the same way. This (indefinite numbers of arguments) is a valuable 
; thing.

; Yet the particular Lisp system that I showed you (in Metacircular Evaluator, Part 1) 
; is one where the numbers of arguments is fixed, because I had to match the arguments 
; against the formal parameters in the binder, where there's a pair up. Well I'd like to be
; able new procedures like this that can have any number of arguments.

; Several parts of the problem: first part is SYNTACTIC SPECIFICATION. Then the other thing
; is once we notated it how we are going to INTERPRET that notation so as to DO THE RIGHT
; THING as whatever that right thing is.

; An example might be this:

(lambda (x . y) ; a procedure of one argument x where one x is required and there are 
                ; many y's. y will be the list of them

; We can then do something like this: 
    
(map (lambda (u) (* x u)) y) ; That procedures of one argument u which multiplies 
                             ; x by u and applies that to y.
                             
; I've used a dot here to explain that what comes next is a list of all the arguments. 
; That is a syntactic specification. Now, what this depends upon, the reason why this is
; sort of a reasonable thing to do, is because this happens to be a syntax that's used
; in the Lisp reader for representing conses. 

; We've never introduced that yet. But you might have seen in the system that if you cons
; two things together, you get the first, space, dot, the second, space-- the first, 
; dot, space, the second, and parentheses around the whole thing.

; If I have list of arguments I wish to match [a list of three things] against I can
; easily match it against another list of three. But now, supposing I were to compare
; this x dot y -- (x . y) -- supposing I compare that to a list of three arguments one, two,
; three. I can walk along here and so yes, x matches the 1, the y matches the list which is
; two and three. So the notation I'm choosing is one that is very natural for the Lisp
; system. It is chosen as a notation for a bunch of arguments.

; Now, there's an alternative possibility if I don't want to take one special out or two
; special ones out or something like that, if I don't want to do that, if I just want to
; talk about the list of all the arguments like in addition, well then the argument list
; is going to be all the arguments [represented by a single symbol]. Like in 
; (lambda x x) = list

; When you make syntactic specifications it is important that it's unambigious, that neither
; of these can be confused with a representation we already have.

; I can always tell I have a fixed number of explicitly named arguments made by these formal
; parameters (1), or a fixed numbers of named formal parameters followed by a thing which
; picks up all the rest of them (2), or a list of all the arguments which will be matched
; against (3). 

1. (lambda (x y z)
2. (lambda (x . y)
3. (lambda x x)

; Many languages make terrible errors in that form where whole segments of interpretation
; are cut off, because there are syntactic ambiguities in the language. There are
; traditional problems in languages like ALGOL that have to do with the nesting of ifs
; in the predicate part. 

; In any case, now, so I've told you about the syntax, now, what are we going to do about
; the semantics of this. How are are going to interpret it? Well this is just super easy. 
; I'm going to modify the metacircular interpreter to do it. And that's a one liner. Here
; it is:

(define pair-up                                    ; Here's the procedure that pairs the
  (lambda (vars vals)                              ; formal parameters with the arguments
    (cond                                          ; that were passed. 
      ((eq? vars '())                              ; This is the same, if the list of vars
       (cond ((eq? vals '()) '())                  ; is empty, then if the list of vals is
             (else (error "Too many arguments")))) ; empty, then I have an empty list.
      ((symbol? vars)                              ; Otherwise I have TMA. If the vars are a
       (cons (cons vars vals) '()))                ; symbolic tail, a tail with a symbol, y
                                                   ; (lambda (x . y)) (not the empty list)
                                                   ; in that case I wish to match that vars
                                                   ; to the pairing that I'm making.
      ((eq? vals '()) (error "Too few arguments")) ; If I have empty vars but not vals I
      (else                                        ; have TFA.
        (cons (cons (car vars)                     ; Otherwise I go to the whole arrangement
                    (car vals))                    ; of making up the whole pairing.
              (pair-up (cdr vars)
                       (cdr vals)))))))

; Questions?

; Q: Could you explain that third form (lambda x x)
; A: Let's look at it like a piece of list structure. This is a procedure which contains
;    a lambda, x and x. If I were looking for the bound variable list part of this procedure
;    I would go looking at the cadr, and I'd find a symbol. So the, naturally, pair-up 
;    thing I just showed you, is going to be matching a symbolic object against a list of
;    arguments that were passed. And it will that symbol to the list of arguments.
;    In the case (lambda (x . y)) when I am looking for it, the match will be against 
;    (x . y) in the bound variable list position. Now, if what this does is get the list
;    of arguments and returns it, that's list. That's what the procedure is. 

