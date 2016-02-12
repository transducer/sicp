#lang scheme

; When we learned about streams we worried about the order of evaluation and delayed
; arguments to procedures. The way we played with streams was that it was the responsibility
; of the caller and the callee to both agree that an argument was delayed and the callee 
; must force the argument if it needs the answer. So there had to be a lot of hand shaking
; between the designer of a procedure and a user of it over delayedness.

; That turns out to be a fairly bad thing. It works alright with streams, but as a general
; thing what you want is an idea to have a locus, a decision, a design decision in general.
; To have a place where the choice is made explictly, and notated in a clear way.

; So it is not a very good idea to have to have an agreement between the person who writes
; a procedure and the person who calls it about such details as the order of evaluation.
; It would be nice if one of these could take responsibility completely.

; This is not a new idea. ALGOL 60 had two different ways of calling a procedure. The 
; arguments could be passed by name or by value. What that meant is that a name argument
; was delayed: that when you passed an argument by name, that its value would only be 
; obtained if you accessed that argument. 

; So what I would like you to show you is how to make a modification to a language to add
; a feature "by name parameters" or "delayed parameters". Because in fact the default in our
; Lisp system is by the value of a pointer. A pointer is copied, but the data structure
; it points at is not. I show you how to make name arguments as well.

; Again: why would we need such a thing? 

; Well supposing we wanted to invent certain kinds of what otherwise would be special forms,
; reserve words. I want procedures that can do things like if. if is special, or cond, or
; whatever it is, it's the same thing. It's in that it determines whether or not to 
; evaluate the consequent or the alternative based on the value of the predicate part of an
; expression. 

; Whereas all the procedures like + or others we can define evaluate all of their arguments
; before application. 

; Suppose I want to be able to define the reference of if in terms of if, let's call it
; unless:

(define (unless p c a) ; We have a predicate, a consequent and an alternative.
  (cond ((not p) c)    ; if not the predicate, then take the consequent,
        (else a)))     ; otherwise, take the alternative

; What I'd like this to mean is suppose I want to do something like this:

(unless (= 1 0) 2 ( / 1 0))

; I'd like that to transform into or mean:

(cond ((not = 1 0) 2)
      (else (/ 1 0)))
; => 2

; Because all of the arguments are evaluated before I start, our unless gives an error
; (division by 0).

; I don't get the wrong answer, I get no answer. I would like to make a definition so that
; this works. I want to be able to say something special about the consequent c and 
; alternative a, I want them to be delayed automatically. I don't want them to be evaluated 
; at the time of the call.

; So we are going to make a declaration and then we are going to see how to implement such
; declaration.

; Again, I want you to say to yourself this is an interesting kluge he's adding in here.
; The piles of kluges that make a big complicated mess. And is this going to foul up
; something else that might occur? First of all, is it syntactically unambiguous? Well, it
; will be with what we have seen so far, but it may in effect cause trouble. It might 
; conflight with type declarations I might add in the future. To give some compiler the
; ability to optimize given the types are known. Or it might conflict with other types of
; declarations I might want to make about the formal parameters. So I'm not making a 
; general mechanism here where I can add declarations. I'd like to do that, but I don't want
; to talk about that right now. 

; I am going to build a kluge:

(define (unless p (name c) (name a))
  (cond ((not p) c)
        (else a)))

; Where I can explicitly declare certain parameters to be delayed, to be computed later.

; Now this is actually a very complicated modification to an interpreter, rather than a
; simple one. Dynamic binding or adding indefinite arguments procedures is relatively 
; simple. But this one changes a basic strategy. The problem here is that our interpreter,
; as written, evaluates a combination by evaluating the procedure, the operator producing
; the procedure, and evaluating the operands producing the arguments, and then doing
; apply of the procedure to the arguments. However, here, I don't want to evaluate the
; operands to produce the arguments until after I examined the procedure to see what the 
; procedures declarations look like.

; Let's look at that. Here we have a changed evaluator:

(define eval
  (lambda (exp env)
    (cond
      ((number? exp) exp)
      ((symbol? exp) (lookup exp env))
      ((eq? (car exp) 'quote) (cadr exp))
      ((eq (car exp) 'lambda)
       (list 'closure (cdr exp) env))
      ((eq? (car exp) 'cond)
       (evcond (cd exp) env))
      (else
        (apply (undelay (eval (car exp)
                              env))
               (cdr exp)
               env)))))

; I'm starting with the simple lexical evaluator, not dynamic, but we're going to have to
; do something sort of similar in some ways. Because of the fact that if I delay an 
; argument to a procedure, I'm going to have to attach an environment to it. Remember how
; Hal implemented delay. Hal implemented delay as being a procedure of no arguments
; which does some expression. That's what delay of an expression is.

(delay e) ; => (lambda () e)

; Now if I evaluate a lambda expression I have to capture the environment. The reason why 
; this is, is because there are variables in e which meaning I want to arrive from the 
; context where this was written. So that's why a lambda does the job, it's the right thing.
; Evaluating in such a way that the forcing of a delayed expression was the same thing as 
; calling that with no arguments:

(force e) ; => (e)

; Producing an environment of the call which was in fact the environment where e was 
; defined. With an extra frame that was empty, I don't care about that.

; Well, if we go back to the above eval, since it is the case that everything is the same
; as it was before, except the case of applications or combinations. And combinations are
; going to do two things: 1) Evaluate the operator and making sure that it is current, that
; it is not a delayed object (by calling undelay), and evaluate to the point where it's 
; forced, and 2) Somehow apply that to the operands (cdr exp) where I have to pass the
; environment along so some of those operands I may have to delay. I may have to attach
; that environment to those operands.

; This is a rather complicated thing happening. 

; Looking at that in apply:

(define apply
  (lambda (proc ops env)
    (cond
      ((primitive? proc)                    ;magic
       (apply-primop proc
                     (evlist ops env)))
      ((eq? (car proc) 'closure)
       ;; proc = (CLOSURE (bvrn body) env)
       (eval (cadadr proc)                  ;body
             (bind (vnames (caadr proc))
                   (gevlist (caadr proc)
                            ops
                            env)
                   (caddr proc))))          ;env
      (else (error "Unknown procedure")))))    

; Apply has a primitive procedure thing just like before. But the compound is a little more
; interesting. I have to evaluate the body just as before, in an environment which is the
; result of binding some formal parameters to arguments in the environment. The environment
; is one that comes from the procedure now. It's a lexical language, statically bound. 
; However, one thing I have to do is strip off the declarations of the name of the variables
; (that's what vnames does). And the other thing is process these declarations, deciding 
; which of these operands -- that's the operands now, as opposed to the arguments -- to
; evaluate and which of them are to be encapsulated in delays of some sort. 
;
; The other thing you see here is that a primitive (like +), had better get at the real 
; operands. So there is a place where we're going to have to force them. So evlist has to
; do a bunch of forces. So we have two different kind of evlists now: evlist and gevlist.

; gevlist is going to wrap delays around some things and force others, evaluate others.

; evlist is going to do some forcing of things.

(define evlist
  (lambda (l env)
    (cond
      ((eq? l '()) '())
      (else
        (cons (undelay (eval (car l) env))
              (evlist (cdr l) env))))))

; Just looking at this a little bit, this is a game you must play for yourself you know.
; It's not something that you're going to see all possible variations on an evaluator
; talking to me. What you have to do is do this for yourself, and after you feel this, you
; play this a bit, you get to see all the possible design decisions and what they might 
; mean, and how they interact with each other. So what languages might have in them. And 
; what are the consistent sets that make a legitimate language. Whereas what things are
; just complicated kluges that are just piles of junk.

; So evlist over here, just as I said, is a list of operands which are going to be 
; undelayed after evaluation. So these are going to be forced, whatever that's going to 
; mean.

; And gevlist:

(define gevlist
  (lambda (vars exps env)
    (cond
      ((eq? exps '()) '())
      ((symbol? (car vars)) ;applicative
       (cons (eval (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      ((eq? (caar vars) 'name)
       (cons (make-delay (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      (else (error "Unknown declaration")))))

; And gevlist is a couple of possibilities. Either it's a normal ordinary thing, a symbol
; sitting there like the predicate in the unless, and that's what we have at symbol?. 
; In which case it is intended to be evaluated in applicative order. And it's, essentially,
; just what we had before. It's mapping eval down the list. In other words, I evaluate
; the first expression and continue gevlisting the cdr of expression in the environment.

; However, it's possible that this is a name parameter (see (eq? (caar vars) 'name) above).
; If it's a name parameter, I want to put a delay in which combines that expression, which
; I'm calling by name, with the environment that's available at this time and passing
; that as the parameter. That's part of the mapping process in gevlist.

; The only other interesting place in this interpreter is cond:

(define evcond
  (lambda (clauses env)
    (cond
      ((eq? clauses '()) '()) ;arbitrary
      ((eq (caar clauses) 'else)
       (eval (cadar clauses) env))
      ((false? (undelay
                 (eval (caar clauses)
                       env))))
      (evcond (cdr clauses) env))
    (else
      (eval (cadar clauses) env))))

(define false?
  (lambda (x) (eq? x '())))

; Conditionals have to know whether or not the answer is true or false. It's like a 
; primitive. When you do a conditional, you have to force.

; What is left is how you make delays:

(define make-delay
  (lambda (exp env)
    (cons 'thunk (cons exp env))))

(define (undelay v)
  (cond ((pair? v)
         (cond ((eq? (car v) 'thunk)
                (undelay
                  (eval (cadr thunk)
                        (cddr thunk))))
               (else v)))
        (else v)))

; Well, delays are data structures which contain an expression, an environment, and a type
; on them. And it says they are "thunk". That comes from the ALGOL language, and it's
; claimed to be the sound of something being pushed on a stack. I don't know. I was not an
; ALGOLician or an ALGOLite or whatever, so I don't know. But that's what claimed.

; And undelay is something which will recursively undelay thunks until the thunk becomes
; something which isn't a thunk. 

; This is the way you implement a call by name.

; Questions?

; Q: I noticed you avoided by name in the primitive procedures, I was wondering what 
;    caused you to do that?
; A: The question is if it's ever reasonable to call a primitive procedure by name.
;    And the answer is "Yes".
;    There are two particular cases where it is reasonable. While constructing a data 
;    structure like cons or making an array if you have arrays with any number of elements,
;    it's unnecessary to evaluate those arguments. All you need is promises to evaluate
;    those arguments if you look at them. If I cons together two things, then I could cons
;    together the promises just as easily as I could cons together the things. And it's not
;    even when I car or cdr them I have to look at them. That just gets out the promises and
;    passes them to somebody. That's why the lambda calculus definition, the Alonzo Church
;    definition of car, cdr and cons makes sense. It's because no work is done in car, cdr,
;    and cons, it's just shuffling data, it's just routing, if you will. However, the things
;    that do have to look at data are things like plus. Because they have a look at the bits
;    that the numbers are made of, unless they're lambda calculus numbers which are funny.
;    They have to look at the bits to be able to crunch them together and do the add.
;    So in fact, data constructors, data selectors, things that side-effect data objects,
;    don't need to do any forcing in the laziest possible interpreters. On the other hand,
;    predicates on data structures have to. Is this a pair? Or is it a symbol? Well you have
;    to look at it then.

