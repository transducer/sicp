#lang scheme

; So far we have invented enough programming to do some very complicated things. You 
; certainly learned the most important tricks that usually don't get taught to people until
; they have a lot of experience. For example, data-directed programming is a major trick.
; And yesterday we interpreted language. 

; We did this all in a computer language in which there was no assignment statement. 
; Presumably for those who have seen BASIC or PASCAL or whatever, that is usually considered
; the most important thing.

; Well, today we will do something horrible. We will add an assignment statement. Since we
; could do all these wonderful things without it, why should we add it? An important thing 
; to understand is that today we are going to obey our rule:

; We only add a feature if we have a good reason to add the feature.

; So far we have written functional programs:

;    Functional programs
; encode mathematical truths

(define (fact n)
  (cond ((= n 1) 1)
        (else (* n (fact (- n 1))))))

; The factorial program is basically two clauses. You can write it in mathematical logic as:

; n = 1 -> n! = 1
; n > 1 -> n! = n(n - 1)!

; True statements. These processes evolved by such programs can be understood by substitution:

(fact 4)
(* 4 (fact 3))
(* 4 (* 3 (fact 2)))
(* 4 (* 3 (* 2 (fact 1))))
(* 4 (* 3 (* 2 1)))
(* 4 (* 3 2))
(* 4 6)
24

; Substitution of the formal parameters in the body of a procedure is the execution implied.
; Basically, a sequence of equalities. We are preserving truth.

; There might be more than one organisation of these truth statements. Methods may be  
; distinguished by the choice of truths expressed:

(define (> n m)
  (cond ((= n 0) m)
        (else (1+ (> -1+ n) m))))

; n = 0 -> n + m = m
; n > 0 -> n + m = ((n - 1) + m) + 1

; And also a program that evolves an iterative process:

(define (> n m)
  (cond ((= n 0) m)
        (else (> (-1+ n) (1+ m)))))

; n = 0 -> n + m = m
; n > 0 -> n + m = (n - 1) + (m + 1)

; Two things that compute the same answer, and we have equivalent mathematical truths.
; The way you arrange the truths determines the way in which the process is evolved. So we
; have the flexibility of talking about the function which is computed and the method by
; which it is computed. We need more.

; Today I am going to do this awful thing and introduce the assignment operation. Now, what
; is this. First of all there is going to be another kind of statement. This is set!. Things
; that do stuff like that have an exclamation mark behind it. The exclamation mark is like
; the question mark in that it is an arbitrary symbol we use to signify it does something. It
; has no significance to the system. The only significance is to me and you to alert us that
; this is an assignment of some sort.

<before>
(set! <var> <value>) ; we set a variable to a value.
<after>

; There is a time that something happens. Time progresses, and an assignment is the thing 
; that produces a before and an after. In all the other programs we have written that have no
; assignments in them the order in which they were evaluated did not matter. Assignment is
; special. It introduces a moment in time.

; There is a moment before the set! and after. In that moment in time the variable e <var> has
; the value <value>. This is independent of what value it had before. set! changes the value
; of the variable. Until this moment we had nothing that changed. So for example, one of the
; the things we could think of was that the procedures we wrote for factorial were in fact
; pretty much identical to the function for factorial. Independent of what context it is in
; and independent of how many times I write it, I always get the same answer. Always 24.
; It is unique path from argument to answer. All the programs so far are like that.

; However, once we have an assignment that is no longer true. If I were to:

(define count 1)          ; define count to be 1, and also

(define (demo x)          ; define a procedure demo 
  (set! count (1+ count)) ; which sets x to x + 1 (my god, this looks just like FORTRAN!)
  (+ x count ))           ; then I can try this procedure and run it.

; => (demo 3)             ; count is currently 1 and it gets imcremented.
; 5

; => (demo 3)             ; now count is 2, it is not 1 anymore
; 6

; What we see is that the same expression leads to two different answer. Depending upon time.

; demo is therefore not a function, it does not compute a mathematical function. This is the
; first place where the substitution model is not going to work. This kills the substitution
; model (philophers might argue it was not completely valid at first too, but that is another
; story). But now it is dead for sure:

; Suppose I wanted to use the substitution model to substitute for count. It is not the same
; count. The substitution model is a static phenomenon: it describes things that are true.
; Not truths that change.

; Before I give you an understanding of this I want to let you know this is very bad. We have
; lost our model of computation. Pretty soon I will have to build you a new model of 
; computation. Of course what you already see from this informal play is that the model we are
; going to need is different from the model that I had before in that the variables (like
; count and x) are no longer going to refer to the values they have, but rather to some sort
; of place where the value is stored.

; This is going to cause a lot of trouble. The very fact we are inventing this bad thing means
; there should better be a good reason for it, otherwise it is just a waste of time and a lot
; of effort.

; Suppose I write down the functional style (old style) of factorial in an iterative process.

(define (fact n)
  (define (iter m i)
    (cond ((> i n) m)                     ; m is the product I am accumulating
          (else (iter (* i m) (+ i 1)))))
  (iter 1 1))                             ; start it up

; Above could be a purely mathematical function without time. Now let's make a similar type
; of program using the same algorithm, but with assignments. The imperative version:

(define (fact n)
  (let ((i 1) (m 1))
    (define (loop)
      (cond ((> i n) m)
            (else 
             (set! m (* i m))
             (set! i (+ i 1))
             (loop))))
    (loop)))

; We see two local variables (i and m) and we test if i is greater than m, if so the result is
; m. However, if I am not at the end of the loop I am going to change the product by
; multiplying it by i. 

; In the substitution model we copy the body of the procedure with the arguments substituted
; for the formal parameters. In the imperative model we do not worry about copying, but we
; change the value of m. And also then change the value of i to i + 1 and go buzzing around.

; It is essentially the same program. But there is a way of making errors that did not exist
; until today. For example, if I would do the horrible thing of not being careful and 
; running the program and interchange the two assignments, the program would not compute the
; same function. There is a dependency in time: m depends on i. If I change i first then I got
; the wrong value of i when I multiply it by m. This is a bug that was not available until 
; this moment. Until we introduced time in it.

; As I said, first we need a new model of computation. And second we need a damn good reason
; for doing this kind of ugly thing. Questions?

; We have introduced set! now. But we had let before and we had define before. I am confused
; about the difference between the three. Wouldn't define work in the same way as set if 
; you introduce it?
; => No, define is intended for setting something once the first time. For making it. You have
;        never seen me writing on a blackboard two defines in a row, who's intention was to
;        change the old value of some variable to a new one.

; Is that by convention?
; => No, it is intention. The answer is that for example, internal to a procedure, two
;        defines of the same variable in a row are illegal. define happens once on anything.
;        In interactive debugging you will redefine things and there is an exception made.
;        But define is intended to mean setup something which is forever that value.
;        As if all the defines were done at the beginning. You can only define after a 
;        lambda expression, which is the body of a procedure. 
;        Now let, let does nothing like either of that. It happens exactly once. That context
;        only exists throughout a specific scope. You don't think of let as setting a value
;        again. i never changes because of the let. i gets created because of the let.
;        The let is in fact a very simple idea:

(let ((var1 e1) (var2 e2))
  e3)
; =>

((lambda (var1 var2)
   e3) 
 e1
 e2)

;        This is perfectly understandable from a substitution point of view. It is really 
;        the same expression written in two different ways. In fact the way the actual 
;        system works is that it gets substituted.

; What about difference between define and let?
; => A define is a syntactic sugar whereby essentially a bunch of variables are created by
;    lets and are then setup once. 
