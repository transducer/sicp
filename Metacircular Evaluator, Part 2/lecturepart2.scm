#lang scheme

; Let's see, now I'm going to tell you about a rather more substantial variation, one
; that's a famous variation that many early Lisps had. It's called DYNAMIC BINDING OF
; VARIABLES. And we'll investigate a little bit about that right now.

; I'm going to first introduce this by showing you the sort of idea that would make someone
; want this idea. 

; Suppose for example we looked at the sum procedure, for summing up a bunch of terms.

(define sum                   ; To be that procedure, of a term, lower bound, method of 
  (lambda (term a next b)     ; computing the next index, and upper bound, such that if a
    (cond ((> a b) 0)         ; is greater than b then the result is 0, otherwise, it's 
          (else               ; the sum of the term procedure applied to a, and result of
            (+ (term a)       ; adding up terms, with the next a being a, the next 
               (sum term      ; procedure passed along, and the upper bound being passed
                    (next a)  ; along.
                    next
                    b))))))   ; blink, blink, blink, blink, blink, blink  

; I can use this sum procedure for example like this:

(define sum-powers
  (lambda (a b n)                ; lower bound, upper bound and n
    (sum (lambda (x) (expt x n)) ; we are adding up x to the n, x takes on values from
         a                       ; a to b, imcrementing by one.
         1+
         b)))

; I can also write the product sum:

(define product-powers
  (lambda (a b n)
    (product (lambda (x) (expt x n))
             a
             1+
             b)))

; Now, there's some sort of thing here that should disturb you immediately:

; These look the same.

; Why am I writing this code so many times? Here I am, in the same boat I've been in before.
; Wouldn't it be nice to make an abstraction here?

; What would be a good abstraction? Well I see some code that is identical. 
; Maybe I can pull out (lambda (x) (expt x n))

; Someone might be wanting to write a slightly different procedure which looks like this:

(define sum-powers
  (lambda (a b n) ; <==== THAT n 
    (sum nth-power a 1+ b)))

; And similarly I might want to write the product powers this way, abstracting out this
; idea. I might want this:

(define product-powers
  (lambda (a b n) ; <==== THAT n 
    (product nth-power a 1+ b)))

; And similarly I'd like to define nth-power as:

(define nth-power
  (lambda (x)
    (expt x n))) ; <==== THIS n 

; But I have a problem: my environment model, that is my means of interpretation for the
; language that we have defined so far does not give me a meaning for THIS n (see above). 
; Because, as you know, THIS n is free in this procedure.

; The environment model tells us that the meaning of a free variable is determined in the
; environment in which this procedure is defined. In a way I have written it I am
; assuming that the way it is defined is on the blackboard as is this is defined in the 
; global environment, where there is no n. Therefore, n is an unbound variable. But it's 
; perfectly clear, to most of us, that we would like it to be THAT n, and THAT n 
; (see above).

; On the other hand, it would be nice, that we are careful to keep the outer n to be
; the outer one. The desire to this work has led to a very famous bug.

; THE FAMOUS *BUG.* 

; Dynamic binding -- a free variable in a 
; procedure has its value defined in the
; chain of callers, rather than where the
; procedure is defined.
;
; Easy to implement -- but....

; The value of a free variable is defined in the environment of the caller of a procedure
; (in dynamic binding) instead of the environment of definition of a procedure.

; So what you have is a system where you search up the chain of callers of a particular
; procedure. And of course, in the case of nth-power, where it is called from inside
; product and product is presumably called from product powers, and since product powers
; binds the variable n, then nth-powers' n would be derived through that chain.

; Similarly, the nth-power of n in the sum case would come through nth-power sum (term)
; which bound n. Therefore, there would be an n available for THIS n to get its value from.

; That's what called the dynamic binding view of the world. If that works.

; Now, let's take a look, for example, what it takes to implement that. It is very easy
; to implement. In fact, the very first Lisps that had any interpretation of free
; variables at all had dynamic binding interpretation for the variables. So has APL, not
; lexical or static, but dynamic.

; Of course this changes eval. And it's really in two places:

(define eval
  (lambda (exp env)
    (cond
      ((number? exp) exp)
      ((symbbol? exp) (lookup exp env))
      ((eq? (car exp) (lookup exp env))
       ((eq? (car exp) 'quote) (cadr exp))
       ((eq? (car exp) 'lambda) exp)       ;! 1)
       ((eq? (car exp) 'cond)
        (evvond (cdr exp) env))
       (else
         (apply (eval (car exp) env)
                (evlist (cdr exp) env)
                env))))))                  ;! 2)

; First of all, one thing we see it that things become a little simpler. If I don't have
; to have the environment be the environment of definition for a procedure, the procedure
; does not need to capture the environment at the time that it's defined.

; 1) We see that the clause for a lambda expression, which is the way in which a procedure 
; is defined, does not make up a thing which has a type closure and an attached environment 
; structure, it is just the expression itself. And we'll decompose that in some other way 
; somewhere else.

; 2) The other thing we see is the applicator must be able to get the environment of the
; caller. The caller of a procedure is right here. If the expression we're evaluating is
; an application or a combination, then we are going to call a procedure which is the value
; of the operator. The environment of the caller is the environment we have right here,
; available right now (coming from the parameter of eval). So all I have to do is pass that 
; environment to the applicator, to apply.

; If we look at that, the only change we have to make are:

(define apply
  (lambda (proc args env)                 ;! Take that environment
    (cond
      ((primitive? proc)                  ;magic
       (apply-primop proc args))
      ((eq? (car proc) 'lambda)
       ;; proc = (LAMBDA bvrs body)
       (eval (caddr proc)                 ;body
             (bind (cadr proc)            ;bvrs (bound variables)
                   args
                   env)))                 ;env! And use that environment for the purpose of
      (else error "Unknown procedure")))) ;     extending that environment when binding the
                                          ;     the formal parameters of the procedure
                                          ;     (proc) to the arguments that were passed
                                          ;     (args), which is not an environment that 
                                          ;     was captured in the procedure.

; The reason why the first Lisps were implemented this way, is that it is sort of the 
; obvious, accidental implementation. And of course as usual, people got used to it and 
; liked it. And there were some people who said "This is the way to do it!".

; Unfortunately dynamic binding like this causes some serious problems. The most important 
; serious problem in using dynamic binding is there's a modularity crisis that's involved 
; in it. If two people are working together on some big system, then an important thing to 
; want is that the names used by each one don't interfere with the names of the other. It's 
; important that when I invent some segment of code, that no one can make my code stop 
; working by using my names that I use internal to my code, internal to his or her code.

; However, dynamic binding violates that particular modularity constraint in a clear way.
; Consider, for example, what happens in sum. Suppose that I decided to change the word
; "next". Supposing somebody is writing sum, and somebody else is going to use sum. The
; writer of sum has a choice of what names he can use. Well, I'm that writer. Well, by
; gosh, just happens I didn't want to call this next, but I called it n. 

; Whoops.. I changed nothing about the specification of the program sum, but the programs
; that makes use of nth-power stop working. Why do they stop working? Well, instead of
; chasing out the value of the n that occurs in nth-power, through the environment of
; definition, where the n that is passed in is always linked to the one passed to the
; method, the lambda is executed in the environment where the n is defined. 

; If I have to chase through the call chain, then look what horrible thing happens. Well, 
; sum was called as term, term a, I'm looking for a value of n, instead of the n passed to 
; the lambda of nth-powers, I get the one passed to sum.

; So by changing the inside of one program, the other stops working. This means we do no 
; longer have a quantifier as described before. A quantifier that has the property that the
; names that are bound by it are unimportant, that I can uniformly substitute any names
; throughout so long as they don't occur in the scope, and the meaning of the expression
; doesn't change. Now I just changed the meaning of an expression by changing the name. So
; lambda is no longer a well-defined idea. That is a very serious problem.

; So for that reason, I and my buddies have given up this particular kind of abstraction 
; of dynamic binding (in nth-power), in favor of a modularity principle. 

; This is the kind of experiment you can do if you want to play with these interpreters. 
; You can try them out this way, that way, and the other way. You see what makes a nicer
; language. So that's a very important thing to be able to do.

; Now I want to give you a feeling for what the RIGHT THING to do is here. How are you going
; to get this kind of power (dynamic binding) in a LEXICAL SYSTEM.

; The answer is, what I REALLY WANT, is something that makes up for me an exponentiator for
; a particular n. Given an n, it will make me an exponentiator.

; But that's easy too. In other words, I can write my program in this way:

(define pgen                  ; Which is a procedure of 
  (lambda (n)                 ; n which produces for me an
    (lambda (x) (expt x n)))) ; exponentiator -- x to the n

; Given that I have that, then I can capture the abstraction that I wanted even better, 
; because it is now encapsulated in a way where it can't be destroyed by a change of
; names. 

(define sum-powers          ; I can define sum-powers to be a procedure again of a, b
  (lambda (a b n)           ; and n which is the sum of the term function generated 
    (sum (pgen n) a 1+ b))) ; by using this generator pgen, n, with a, incrementer, and b

(define product-powers       ; and I can define the product of
  (lambda (a b n)            ; powers to be procedure of a, b, and n which is the 
    (product (pgen n) a b))) ; product pgen, n, with a, increment and b 

; Now of course this is a very simple example where this object that I'm trying to abstract
; over is small. But it could be 100 lines of code. So the purpose of this is, of course,
; to make it simple. I'd give a name to it, it's just that here it's a parametrized name.
; It's a name that depends explicitly upon the lexically apparent value of n.

; Questions?

; Q: is the only solution to the problem you raise to create another procedure? In other
;    words, can this only work in languages that are capable of defining objects
;    as procedures?
; A: Oh, I see. My solution to making this abstraction, where I did not want to include
;    the procedure inside the body, depended upon my ability to return a procedure or export
;    one. And that's right. If I dont have that I don't have the ability to make an
;    abstraction in a way where I don't have a way to avoid symbol problems that were 
;    unanticipated.

;    I consider to be able to return a procedural value, and therefore, to sort of have
;    first class procedures in general, as being essential to doing very good modular
;    programming. Indeed there are very many other ways to skin this cat. What you can do is
;    for each of the bad things that you have to worry about, you can make a special
;    feature that covers that thing: You can make a package system; You can make a module
;    system as in Ada; et cetera. And all of those work, or they cover little regions of it.
;    The thing is that returning procedures as values covers all of those problems. And so 
;    it's the simplest mechanism that gives you the best modularity, give you all of the 
;    known modularity mechanisms.

