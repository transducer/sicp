#lang scheme

; Lecture text obtained from (slight modifications made):
; Eric Grimson, Peter Szolovits, and Trevor Darrell, 6.001 Structure and 
; Interpretation of Computer Programs, Spring 2005. (Massachusetts Institute
; of Technology: MIT OpenCourseWare). http://ocw.mit.edu (accessed 11 12, 
; 2015). License: Creative Commons Attribution - Noncommercial - Share Alike.


; When we originally introduced pairs made out of CONS, made by CONS, we only said a few 
; axioms about them, which were of the form: for all X and Y, the CAR of 
; the CONS of X and Y is X and the CDR of the CONS of X and Y is Y. 

Vxy (car (cons x y)) = x
    (cdr (cons x y)) = y
    
; Now, these say nothing about whether a CONS has an identity like a person. In fact, all they say 
; is something sort of abstract, that a CONS is the parts it's made out of. And of course, two
; things are made out of the same parts, they're the same, at least from the point of view of 
; these axioms. 

; But by introducing assignment - in fact, mutable data is a kind of assignment, we have a 
; set CAR and a set CDR - by introducing those, these axioms no longer tell the whole
; story. 

; And they're still true if written exactly like this. But they don't tell the whole story. 
; Because if I'm going to set a particular CAR in a particular CONS, the questions are, well, is 
; that setting all CARs and all CONSes of the same two things or not? If I -if we use CONSes to 
; make up things like rational numbers, or things like 3 over 4, supposing I had two three-
; fourths. Are they the same one - or are they different? 

; Well, in the case of numbers, it doesn't matter. Because there's no meaning to changing the 
; denominator of a number. What you could do is make a number which has a different denominator. 
; But the concept of changing a number which has to have a different denominator is sort of a 
; very weird, and sort of not supported by what you think of as mathematics. However, when these 
; CONSes represent things in the physical world, then changing something like the CAR is like 
; removing a piece of the fingernail. And so CONSes have an identity. 

; Let me show you what I mean about identity, first of all. Let's do some little example here. 
    
(define a (cons 1 2))
(define b (cons a a))

; Supposing I define A to the CONS of 1 and 2. Well, what that means, first of all, is that 
; somewhere in some environment I've made a symbol A to have a value which is a pair 
; consisting of pointers to a 1 and a pointer to a 2, just like that. Now, supposing I also say 
; define B to be the CONS - it doesn't matter, but I like it better, it's prettier - of A and A. 

; Well, first of all, I'm using the name A twice. At this moment, I'm going to think of CONSes 
; as having identity. This is the same one. And so what that means is I make another pair, 
; which I'm going to call B. And it contains two pointers to A. At this point, I have three 
; names for this object. A is its name. The CAR of B is its name. And the CDR of B is its name.
; It has several aliases they're called. 

; Now, supposing I do something like set-the-CAR, the CAR of the CAR of B to 3. What that 
; means is I find the CAR of B, that's this. I set the CAR of that to be 3, changing this. I've 
; changed A. If I were to ask what's the CAR of A now? I would get out 3, even though 
; here we see that A was the CONS of 1 and 2. 

(set-car! (car b) 3)

(car a)
; => 3

; I caused A to change by changing B. There is sharing here. That's sometimes what we want. 
; Surely in the queues and things like that, that's exactly what we defined our - organized our 
; data structures to facilitate - sharing. But inadvertent sharing, unanticipated interactions 
; between objects, is the source of most of the bugs that occur in complicated programs. So 
; by introducing this possibility of things having identity and sharing and having multiple 
; names for the same thing, we get a lot of power. But we're going to pay for it with lots of 
; complexity and bugs. 

; So also, for example, if I just looked at this just to drive that home, the CADR of B, which
; has nothing to do with even the CAR of B, apparently. The CADR of B, what's that? Take 
; that CDR of B and now take the CAR of that. Oh, that's 3 also. So I can have non-local 
; interactions by sharing. And I have to be very careful of that. 

(cadr b)
; => 3

; Well, so far, of course, it seems I've introduced several different assignment operators -
; set, set CAR, set CDR. Well, maybe I should just get rid of set CAR and set CDR. Maybe 
; they're not worthwhile. Well, the answer is that once you let the camel's nose into the tent,
; the rest of him follows. All I have to have is set, and I can make all of the - all of the bad 
; things that can happen. 

; Let's play with that a little bit. A couple of days ago, when we introduced compound data, 
; you saw Hal show you a definition of CONS in terms of a message acceptor. I'm going to 
; show you even a more horrible thing, a definition of CONS in terms of nothing 
; but air, hot air. What is the definition of CONS, of the old functional kind, in terms of purely 
; lambdic expressions, procedures? Because I'm going to then modify this definition to get 
; assignment to be only one kind of assignment, to get rid of the set CAR and set CDR in terms of 
; set.

; So what if I define CONS of X and Y to be a procedure of one argument called a message M, 
; which calls that message on X and Y? 

(define (cons x y)
  (lambda (m) (m x y)))

; This idea was invented by Alonzo Church, who was the greatest programmer of the 20th century, 
; although he never saw a computer. It was done in the 1930s. He was a logician, I suppose at 
; Princeton at the time. 

; Define CAR of X to be the result of applying X to that procedure of two arguments, A and D, 
; which selects A. I will define CDR of X to be that procedure, to be the result of applying X to 
; that procedure of A and D, which selects D.

(define (car x)
  (x (lambda (a d) a)))

(define (cdr x)
  (x (lambda (a d) d)))
  
; Now, you may not recognize this as CAR, CDR, and CONS. But I'm going to demonstrate to 
; you that it satisfies the original axioms, just once. And then we're going to do some playing 
; of games. 

; Consider the problem CAR of CONS of, say, 35 and 47. Well, what is that? It is the result of 
; taking car of the result of substituting 35 and 47 for X and Y in the body of 
; this. Well, that's easy enough. That's CAR of the result of substituting into lambda of M, M 
; of 35 and 47. 

(car (cons 35 47))

(car (lambda (m) (m 35 47)))

((lambda (m) (m 35 47)) (lambda (a d) a))

((lambda (a d) a) 35 47)
; => 35

; Well, what this is, is the result of substituting this object for X in the body of that.
; So that's just lambda of M - that's substituted, because this object is being substituted for X, 
; which is the beginning of a list, lambda of M - M of 35 and 47, applied to that procedure of A 
; and D, which gives me A. Well, that's the result of substituting this for M here. So that's the 
; same thing as lambda of A, D, A, applied to 35 and 47. Oh, well that's 35. That's substituting 
; 35 for A and for 47 for D in A. So I don't need any data at all, not even numbers. This is 
; Alonzo Church's hack. 

; Well, now we're going to do something nasty to him. Being a logician, he wouldn't like this. 
; But as programmers, let's look at the overhead. And here we go. I'm going to change the 
; definition of CONS. It's almost the same as Alonzo Church's, but not quite. 

;     "Lambda Calculus" Mutable Data

(define (cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))

(define (car x)
  (x (lambda (a d sa sd) a)))

(define (cdr x)
  (x (lambda (a d sa sd) d)))

(define (set-car! x y)
  (x (lambda (a d sa sd) (sa y))))

(define (set-cdr! x y)
  (x (lambda (a d sa sd) (sd y))))

; What do we have here? The CONS of two arguments, X and Y, is going to be that procedure of 
; one argument M, which supplies M to X and Y as before, but also to two permissions, the 
; permission to set X to N and the permission to set Y to N, given that I have an N.
; So besides the things that I had here in Church's definition, what I have is that the thing 
; that CONS returns will apply its argument to not just the values of the X and Y that the 
; CONS is made of, but also permissions to set X and Y to new values. Now, of course, just as 
; before, CAR is exactly the same. The CAR of X is nothing more than applying X, as in 
; Church's definition, to a procedure, in this case, of four arguments, which selects out the 
; first one. And just as we did before, that will be the value of X that was contained in the 
; procedure which is the result of evaluating this lambda expression in the environment 
; where X and Y are defined over here. That's the value of CONS. 

; Now, however, the exciting part. CDR, of course, is the same. The exciting part, set CAR 
; and set CDR. Well, they're nothing very complicated anymore. Set CAR of a CONS X to a 
; new value Y is nothing more than applying that CONS, which is the procedure of four- the 
; procedure of one argument which applies its argument to four things, to a procedure which 
; is of four arguments -the value of X, the value of Y, permission to set X, the permission to
; set Y - and using it - using that permission to set X to the new value. And similarly, set-cdr 
; is the same thing. 

; So what you've just seen is that I didn't introduce any new primitives at all. Whether or not 
; I want to implement it this way is a matter of engineering. And the answer is of course I 
; don't implement it this way for reasons that have to do with engineering. However in 
; principle, logically, once I introduced one assignment operator, I've assigned - I've introduced 
; them all. Are there any questions? 

; AUDIENCE: I can follow all of that. But when we bring in the permissions, defining CONS in terms 
; of the lambda N, I don't follow where N gets passed. 

; PROFESSOR: Oh, I'm sorry. I'll show you. Let's follow it. Of course, we could do it on the 
; blackboard. It's not so hard. But it's also easy here. 

; Supposing I wish to set-cdr of X to Y. See that right there. set-cdr of X to Y. X is presumably
; a CONS, a thing resulting from evaluating CONS. Therefore X comes from a place over here, that 
; that X is of the result of evaluating this lambda expression. Right? 

; That when I evaluated that lambda expression, I evaluated it in an environment where the 
; arguments to CONS were defined. That means that as free variables in this lambda expression, 
; there are in the frame, which is the parent frame of this lambda expression, the procedure 
; resulting from this lambda expression, X and Y have places. And it's possible to set them. I set 
; them to an N, which is the argument of the permission. The permission is a procedure which is 
; passed to M, which is the argument that the CONS object gets passed. 

; Now, let's go back here in the set-cdr The CONS object, which is the first argument of set-cdr
; gets passed an argument. That there's a procedure of four things, indeed, because 
; that's the same thing as this M over here, which is applied to four objects. The object over 
; here, SD, is, in fact, this permission. When I use SD, I apply it to Y, right there. So that 
; comes from this. 

; AUDIENCE: So what do you -

; PROFESSOR: So to finish that, the N that was here is the Y which is here. How's that? 

; AUDIENCE: Right, OK. Now, when you do a set-cdr, X is the value the CDR is going to 
; become. 

; PROFESSOR: The X over here. I'm sorry, that's not true. set-cdr has two arguments:
; The CONS I'm changing and the value I'm changing it to. So you have them 
; backwards, that's all. Are there any other questions? 

; Well, thank you. It's time for lunch. 

