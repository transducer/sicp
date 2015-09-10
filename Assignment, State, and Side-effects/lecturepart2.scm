#lang scheme

; I now have to rebuilt the model of computation so you understand how some such mechanical
; process could work in the way we just talked about. I have just recently destroyed your
; substitution model. Unfortunately, this model is significantly more complicated than the
; substitution model. It is called the environmment model. And I am going to have to 
; introduce some terminology. This is very good terminoloy for you to know anyway. It is 
; about names. We are going to give names to the kind of names things have and the way in 
; which they are used. It is a metadescription.

; A pile of unfortunate terminoloy, but we are going to need this to describe the environment
; model. We are about to do some boring dog work here. Let's look at the first:

; We say that a variable, V., is "bound in an expression", E. if the meaning of E is unchanged
; by the uniform replacement of a variable W. not occuring in E. for every occurence of V in
; E.

; Bound variables we are talking about. You've seen lots of them. You may not know it, but
; in your logic you saw Vx Ey P(x,y) 
; (for every x there exists a y such that p is true of x and y).

; This variable x and that variable y are bound, cause the meaning of this expression does
; not depend on the particular letter I use for x and y. If I set for every w. there exists
; a y such that ..., it would be the same sentence.

; Or another case of this is INTEGRAL(0->1) dx/1+x^2. The x is a bound variable. If I change
; it to a t the expression is still the same thing. 1/4 of arctan of 1 or something.

; Bound variables are very common for those who've played a bit with mathematics.

; Now let's go to the programming world. Instead of a quantifier (a symbol that bounds a 
; variable) we are going to use the quantifier lambda as the essential thing that binds
; variables. So we have some nice examples here such that: 

(lambda (y) ((lambda (x) (* x y)) 3))

; That procedure has the property that it has two bound variables in it (x and y). The 
; quantifier lambda binds the y and the other binds the x. Because if I want to take an
; arbitrary expression like w. and replace all y's with w's it is still the same procedure.

; That we have things like that is a kind of modularity. If two people write a program it
; shouldn't matter what names they use internal to their own little machines that they are
; building.

; On the other hand I have some variables that are not bound. Example:

(lambda (x) (* x y)) ; that procedure of one argument x that multiplies x by y.

; In this case y is not bound. Suppose y had the value 3 and z had the value 4, then this
; procedure would be that which multiples its argument by 3. If I were to replace every 
; instance of y by z I would have a different procedure which multiplies every argument that
; is given by 4. In fact, we have a name for such a variable:

; We say that a variable V. is "free in an expression" E. if the meaning of E is changed by
; the uniform replace of a variable B. not occuring in E. for every occurence of V in E.

; That's why the y above is a free variable in that expression. Other examples are:

(lambda (y) ((lambda (x) (* x y)) 3)) ; that procedure of one argument y that uses that 
                                      ; proceduure of one argument x that multiples x by y
                                      ; and uses that on a 3. The free variable is *.

; You see if it as the normal meaning of multiplication, then if I was to replace uniformly
; all asterisks with plusses then the meaning of the expression above would change. That is
; what we mean by a free variable. 

; So far we have learned some logician words which describe the way in which variables are
; used. Now we have to do a little bit more playing around here and I'll tell you about
; the regions in which variables are defined.

; You see we have been very informal about this until now. And of course many of you have
; probably understood very clearly that x that is declared in the lambda is only defined
; within the parentheses. This is called scope.

; If x is a bound variable in E then there is a a lambda expression where it is bound. We call
; the list of formal parameters of the lambda expression the "bound variable list" and we say
; that the lambda expression "binds" the variables "declared" in its bound variable list.
; In addition, those parts of the expression where a variable has a value defined by the
; lambda expression which binds it is called the "scope" of the variable.

; Btw, we can always arrange thing so that we do not need any defines. We will see that in a
; while. It is a magical thing.

; Really the only thing that makes names is lambda. That is his job. What is amazing is that
; we can compute with only lambda. But in any case, a lambda expression has a place where
; it declares a variable (bound variable list). We say that it "binds" the variables declared.
; Those parts of the expression where the variable is defined is called the "scope" of that
; variable.

; Okay, well now we have enough terminology to begin to understand how to make a new model
; of computation. Because the key thing going on here is that we destroyed the substitution
; model and we now have a model which represents the names as referring to places. 

; Because if we are going to change something we need a place where it is stored. You see if
; a name only refers to a value and I try to change the names meaning, well, that's not clear
; because there's nothing shared among all the instances of that name. And what we really mean
; by a name is we fan something out. We give something a name and you have it and you have it
; because I have given you both a reference to it. We'll see a lot about that.

; So let me tell you about environments:

; Heres a bunch of environments:

;                             I
;                       x = 3  
;                       y = 5
;           II          ^   ^                 III
;     z = 6     --------|C D|--------  m = 1
;     x = 7                            y = 2
;      ^                                 ^
;      |A                                |B
;
; A, B, C, D are environments.
; C and D are the same environment.
; I, II, III are frames.
; z and x are bound in II.

; It is a way of doing substitutions virtually. It represents a place where something is 
; stored which is the substitutions which you haven't done. It's the place where everyhing
; accumulates: where the names of the variables are associated with the values they have, 
; such that when you ask "what does this name mean?" you can look it up in an environment.

; So, an environment is a function.

; Or a table, or something like that, but it is a structured sort of table. It is managed
; by a things called frames, frames are pieces of environment and they are chained together
; in some nice ways by what is called parent links or something like that.

; D is the same variable as A. If I change the value of the variable y in I for example, it
; should be visible for all places.

; In the environments the variables are bound. In II also y is bound, and the value of x 
; from frame II is 7. We say that the x in II shadows the x in I.

; From environment B which refer to frame III we have variables m and y bound, and also x.
; The y shadows the y in I, so y is 2, m = 1, x = 3.

; We have a very simple environment structure made of frames. These correspond to the
; application of procedures. We'll see that in a second.

; Now we see this:

;       A                  ^    ^
;   ------------->       < | >< |>
;                          v    v
;                          C    B
;
; A is (a pointer to) a PROCEDURE OBJECT.
; B is (a pointer to) an environment
; C is the code of the procedure.

; A procedure is made out of two parts (like cons), it is the first part that refers to some
; code which can be executed (a set of instructions if you will). And the second part B is an
; environment. The procedure is the whole thing.

; We have to use this to capture the values of the free variables that occur in the procedure.
; If a variable occurs in a procedure it is either bound in that environment or free.
; If it is bound, then the value will be easy to find (it will be in an easy environment to
; get it). If it is free, we going to have to make use of something that goes with the
; procedure that says where to go look for its value. The reasons why are not obvious yet, but
; will be soon.

; A procedure is a composite object of a piece of code and an environment structure.

; Now I will tell you the complete set of rules for evaluation. There is only two of them:

; Rule 1: A procedure object is applied to a set of arguments by constructing a frame, binding
; the formal parameters of the procedure to the actual arguments of the call, and then
; evaluating the body of the procedure in the context of the new environment constructed. The
; new frame has as its enclosing environment the environment part of the procedure being
; applied.

; The first one is about how to apply a procedure to its arguments. x equals 3 is contained
; in the body of a frame. Then the body will have to be evaluated in an environment which is
; constructed by adjoining the new frame we just made to the environment which was part of
; the procedure we just applied.

; Example:

;                ---------------
;                | ENVIRONMENT |<---- (P 3 4)
;                ---------------     |
;  ^  ^                      ^       |
; < >< > SOME PROCEDURE P  --|       ---------------
;  v  v                              | x = 3, y = 4 |
;   |                                ----------------B
;  (lambda (x y) E)
;
; So we have a procedure which takes an environment. And the procedure has a lambda 
; expression which binds x and y and then binds it to some environment E.
; Now I wish to apply that procedure to 3 and 4.
; To do this I will built a frame which makes x equal to 3 and y equal to 4 and connect
; it to the environment, then the new environment B will be evaluated as the body E.
; Now, E may contain references to x and y and other things, x and y will have values right
; there. Other things will have their values in the top environment. How do we get the 
; other frame? That is what we do by the construction of procedures, which is the other rule:

; Rule 2: A lambda expression is evaluated relative to a given environment as follows:
; a new procedure object is formed, combining the text (code) of the lambda expression with
; a pointer to the environment of evaluation.

; The way I get a procedure is by evaluating a lambda expression, by evaluating it I get a
; procedure which I can apply to 3. 

; Analyzing the lambda: 
(lambda (y) ((lambda (x) (* x y)) 3))

; Now this lambda expression is evaluated in an
; environment where y is defined, and I want the body of that to contain a free variable y.
; Now, if I ever want to look up the value of y, I have to know where it is. Therefore the
; creation of the inner procedure which was the result of evaluating that lambda expression
; has captured a pointer to (or remembers) the frame in which y was bound.

; That is what this rule is telling us. So for example:

; If I happen to be evaluating a lambda expression 
(lambda (x y) G) ~ (within) E

; All that means is that I construct a procedure object in E which is some environment, 
; which has a pointer to the environment where the code of that is the lambda expression or
; whatever it translates into, and the procedure contains both. 

; So this produces the environment pointer and it captures the place in where the lambda
; expression was evaluated, where the definition was used to make a procedure.
; To *make* the procedure.

; So it picks up the environment from where the procedure was made, stored it in the procedure
; itself, and then when the procedure is used, the environment where it was defined is
; extended with the new frame. So this gives us a locus for where a variable has a value.
; For example, if there are a lot of things pointing to some environment, then they share
; that place. We'll see more of that shortly.

; Now we have a new model for understanding the execution of programs.

; Questions:
; It is right to say the environment is the link chain of frames?
; => Yes, the environment is a sequence of frames linked together. I like to think about it
;    as the pointer to the first one. Because once you've got that you've got them all.
;
; Is it possible to define a procedure in two different environments such that it will behave
; differently and have pointers to both ...?
; => Yes, the same lambda expression can be evaluated in two different environments producing
;    two different procedures. Each procedure has the same name, and I can read that textual
;    representation and produce two different procedures, each of these procedures has its 
;    own local sets of variables.
