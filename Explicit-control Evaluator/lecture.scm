;; For a full transcript of the lecture see:

; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and
; -interpretation-of-computer-programs-spring-2005/video-lectures/9b-explicit-control-
; evaluator/lec9b_512kb.pdf


; Lecture PART 1

; We have seen some real magic. The magic of building languages:
; - Escher Picture language
; - Digital logic language
; - Query language

; Toy languages that were really used for other simulations.

; They were all based on LISP.

; Lisp is not good for solving any particular problem. Lisp is good for constructing within
; it the right language for solving the problem you want to solve.

; What is Lisp based on? Where does that come from?
; We looked at the Meta-circular evaluator and we saw it was based on Lisp. 

; For now we will make all the magic go away.

; We will implement Lisp in terms of the register-machine.

; FINITE-STATE CONTROLLER <--> DATA-PATHS <--> STACK

; A Lisp evaluator is concrete enough to hold in the palm of your hand.

; To implement all of Lisp on a register-machine is adding all the procedures that are a
; meta-circular evualator and implement them on a register-machine.

; This will be the final and most explicit model of Lisp in this course.

; There are things that the meta-circular evaluator does not explain. Why for example 
; fact takes more space as it grows and fact-iter does not. Here we explain things like how
; arguments are passed to procedures.

; :) -> characters -> READER -> LIST STRUCTURE IN MEMORY 
;                      |
;                    LIST STRUCTURE -> EVAL -> Primitive ops
;                                       -> PRINTER
;                 characters     <-----------

; Register usage in evaluator machine

; EXP expression to be evaluated
; ENV evaluation environment

; FUN procedure to be applied
; ARGL list of evaluated arguments

; CONTINUE place to go next

; VAL result of evaluation

; UNEV temporary register


; Sample evaluator-machine operations (data paths)

(assign val (fetch exp))

(branch
 (conditional? (fetch exp))
 ev-cond))

(assign exp (first-clause (fetch exp)))

(assign val
        (lookup-variable-value (fetch exp)
                               (fetch env)))

; The meta-circular evaluator:

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp)
         (text-of-quotation exp))

        << ... more special forms ... >>

        ((application? exp)
         (apply
          (eval (operator exp) env)
          (list-of-values (operands exp)
                          env)))
        (else
         (error "Unknown expression"))))

; The main thing to remember is that it is doing some sort of case analysis on the type of
; expression. And then in the general case it is an application. There is some tricky
; recursion going on. Eval has to call itself both to evaluate the operator and to evaluate
; all the operands. The easy recursion. And eval calls apply.
; Applying the procedure to the list of arguments:

(define (apply proc args)
  (cond ((primitive-proc? proc)
         (primitive-apply proc args))
        ((compound-proc? proc)
         (eval-sequence
          (proc-body-proc)
          (extend-environment
           (parameters proc)
           args
           (proc-environment proc))))
        (else
         (error "Unknown proc type"))))

; Apply goes around and calls eval again in the general case of extending the environment.
; In the other case it calls the primitive-apply

; That's the EVAL and APPLY loop. That is what want to see.

; THE EVAL/APPLY CYCLE

; EVAL --------> PROCEDURE ARGUMENTS --------> APPLY --------> EXPRESSION ENVIRONMENT --> EVAL

; The two big pieces correspond to this eval and apply.

; Contract that eval-dispatch fulfills
; - The EXP register holds an expression to be evaluated.
; - The ENV register holds the environment in which the expression is to be evaluated.
; - The CONTINUE register holds a place to go next.
; - The result will be left in the VAL register.
;   Contents of all other registers may be destroyed.

; Contract that apply-dispatch fulfills
; - The ARGL register contains a list of arguments
; - The FUN register contains a procedure to be applied.
; - The top of the STACK holds a place to go next.
; - The result will be left in the VAL register. The stack will be popped.
;   Contents of all the other registers may be destroyed.


; Lecture PART 2

; Let's see how expressions are evaluated.

; EXP: 1
; ENV: <e0> (x = 3, y = 4)
; CONTINUE: done

 eval-dispatch
(branch (self-evaluating? (fetch exp))
        ev-self-eval)
(branch (variable? (fetch exp))
        ev-variable)

< ... more specials forms ... >

(branch (application? (fetch exp))
        ev-application)
(goto unknown-expression-error)


ev-self-eval
(assign val (fetch exp))
(goto (fetch continue))

; EXP: x
; ENV: <e0> (x = 3, y = 4)
; CONTINUE: done

; => x->3 DONE

; EXP: (+ x y)
; ENV: <e0> (x = 3, y = 4)
; CONTINUE: done

; We go to application

; Evaluate the operator and then the arguments and apply them. Note that + is bound in the
; environment that e0 is bound to

ev-application
(assign unev (operands (fetch exp)))
(assign exp (operator (fetch exp)))
(save continue)
(save env)
(save unev)
(assign continue eval-args)
(goto eval dispatch)

ev-variable
(assign
 val
 (lookup-variable-value (fetch exp)))
(goto (fetch continue))

eval-args
(restore unev)
(restore env)
(assign fun (fetch val))
(save fun)
(assign argl '())
(goto eval-arg-loop)

eval-arg-loop
(save argl)
(assign
 exp
 (first-operand (fetch unev)))
(branch (last-operand? (fetch unev))
        eval-last-arg)
(save env)
(save unev)
(assign continue accumulate-arg)
(goto eval-dispatch)

; Evaluate dispatch will just leave 3 at the VAL, and we continue with accumulate-arg
accumulate-arg
(restore unev)
(restore env)
(restore argl)
(assign
 argl
 (cons (fetch val) (fetch argl)))
(assign
 unev
 (rest-operands (fetch unev)))
(goto eval-arg-loop)

; And again, but then go to eval-last-arg. Here we do not need the environment anymore.

eval-last-arg
(assign continue accumulate-last-arg)
(goto eval-dispatch)

accummulate-last-arg
(restore argl)
(assign
 argl
 (cons (fetch val) (fetch argl)))
(restore fun)
(goto apply-dispatch)

; We've evaluated the arguments and gone off to apply them:

apply-dispatch
(branch (primitive-proc? (fetch fun))
        primitive-apply)
(branch (compound-proc? (fetch fun))
        compound-apply)
(goto unknown-proc-type-error)

; In this case primitive-apply:

primitive-apply
(assign
 val
 (apply-primitive-proc (fetch fun)
                       (fetch argl)))
(restore continue)
(goto (fetch continue))

; I don't know how to add, I'm just an execution unit.

; We just executed a recursive procedure. A lot of people think you need a stack and
; recursion in an evaluator is because you might be evaluating recursive procedures like
; factorial or Fibonacci. That is not true. We did recursion here and all we did was
; (+ x y). The reason that you need recursion in the evaluator is because the evaluation
; process itself is recursive.

; In other words: The expressions can contain sub-expressions.

; The other thing to notice is that when we are done we are really done. The machine is
; back to its initial state. The expression and the environment is reduced.


; Lecture PART 3

; Now let's look at an procedure that would use a eval/apply loop:

(define (f a b)
  (+ a b))

(f x b)

; eo will be extended with f, a porecure with args a and b and body of (+ a b)
; We'll go through the same process as above, but instead of apply-displatch will now
; point to the procedure data structure

apply-dispatch
(branch (primitive-proc? (fetch fun))
        primitive-apply)
(branch (compound-proc? (fetch fun))
        compound-apply)
(goto unknown-proc-type-error)

; This time we go to compound-apply.

; What did the meta-circular evaluator do?

(define (apply proc args)
  (cond ((primitiveproc? proc)
         (primitive-apply proc args))
        ((compound-proc? proc) ; Evaluate body of procedure in some new arguments where the
         (eval-sequence        ; parameters of the procedure are bound to the arguments
          (proc-body proc)     ; that were passed in and that is used as a new frame to
          (extend-environment  ; extend the procedure environment
           (parameters proc)   ; Therein we evaluate the procedure 
           args
           (proc-environment proc))))
        (else
         (error "Unknown proc type"))))

; e1 A = 3, B = 4 and linked to e0. In that environment we will evaluate

compound-apply
(assign
 exp
 (procedure-body (fetch fun)))
(assign
 env
 (make-bindings (fetch fun)
                (fetch argl)))
(restore continue)
(goto eval-dispatch)

; Now (f x y) is reduced in e0 to (+ a b) in e1. And there is nothing on the stack. At this
; moment the machine does not contain as part of its state the fact that it's in the middle
; of evaluating some procedure called f. There is no accumulated state.
; That's a very important idea.


; Lecture PART 4

; Now we contrast the iterative procedure where space did not build up, with an actual
; recursive process. E.g.,

(fact-rec 5)
(* 5 (fact-rec 4))
(* 5 (* 4 (fact-rec 3)))
(...)

; EXP: (FACT-REC 5)
; ENV: E0

; it will build an environment E1 which is N to 5 and binds to E0. And in this environment
; it will go off and evaluate the body. It will reduce to evaluating the body in E1.

; EXP (* N (FACT-REC (- N 1))
; ENV E1;

; Now we have an application. Save the value of the continue register on the stack. Then
; evaluate the sub parts.

; Evaluation of (*n (fact-rec (- n 1)))

;   Ready to evaluate the operator

; EXP: *
; ENV: E1
; CONTINUE: eval-args

; STACK (n (fact-rec (- n 1))) <UNEV>
;       E1                     <ENV>
;       done                   <CONTINUE>

; When we return from this eval-dispatch call, we'll end up in the FUN register.
; We're going to evaluate some arguments, put that in the ARGL register and evaluate the
; second operand (we skip off computing (- n 1) and stuff like that). This will reduce
; to another call to fact-recursive:

; Ready to evaluate the second operand

; EXP: (fact-rec (-n 1))
; ENV: E1
; CONTINUE: accumulate-last-arg

; STACK: (5)                  <ARGL>
; <primitive *>               <FUN>
; done                      <CONTINUE>

; And this lead to another recursive factorial. But we haven't yet reduced it. There is
; stuff on the stack.

; Second call to fact-rec

; EXP: (fact-rec (-n 1))
; ENV: E2
; CONTINUE: accumulate-last-arg

; STACK: (5)                  <ARGL>
; <primitive *>               <FUN>
; done                      <CONTINUE>

; In a new environment where N = 4, next call:

; Third call to fact-rec

; EXP: (fact-rec (-n 1))
; ENV: E3
; CONTINUE: accumulate-last-arg

; STACK: (4)                  <ARGL>
; <primitive *>               <FUN>
; accumulate-last-arg       <CONTINUE>
; (5)                         <ARGL>
; <primitive *>               <FUN>
; done                      <CONTINUE>

; This goes on and on:

; Fourth call to fact-rec

; EXP: (fact-rec n)
; ENV: E4
; CONTINUE: accumulate-last-arg

; STACK: (3)                  <ARGL>
; <primitive *>               <FUN>
; accumulate-last-arg       <CONTINUE>
; (4)                         <ARGL>
; <primitive *>               <FUN>
; accumulate-last-arg       <CONTINUE>
; (5)                         <ARGL>
; <primitive *>               <FUN>
; done                      <CONTINUE>

; What is on the stack is effectively the operator you're going to apply and the
; parentheses (what you wanted to do is accumulate them) are really sitting on the stack.
; The substitution model is not such a lie.

; How can it manage to evaluate recursive procedures? The evaluator is setup to save only
; that what it needs later.

; You don't need the environment or the argument list any more later. The evaluator is not
; being smart, it is being stupid. It's only going to save what it really needs.

compound-apply
(assign
 exp
 (procedure-body (fetch fun)))
(assign
 env
 (make-binding (fetch fun)
               (fetch argl)))
(restore continue)             ; <= Here's the actual thing that's making it tail recursive.
(goto eval-dispatch) 

; This one was slightly oversimplified since the evaluator did not handle sequences of
; expressions:

compound-apply
(assign
 unev
 (procedure-body (fetch fun)))
(assign
 env
 (make-binding (fetch fun)
               (fetch argl)))
(goto eval-sequence)            ; <= Does the evaluations one at a time.

eval-sequence
(assign exp (first-exp (fetch unev)))
(branch (last-exp? (fetch unev))
        last-exp)
(save unev) (save env)
(assign continue eval-sequence-cont)
(goto eval-dispatch)
eval-sequence-cont
(restore env) (restore unev)
(assign unev) (rest-exps (fetch unev))
(goto-eval-sequence)
last-exp
(restore continue)
(goto eval-dispatch)

; If you wanted to break tail recursion just do not handle last-exp and go to another place.

; For some reason a lot of Lisp evaluators tended to work like that. The iterative
; procedures than build up stack. No clear reason for that.

; Now we have translated the meta-circular evaluator into a register machine language and
; we have thereby implemented all of Lisp. That's all we did. No more magic in the system.
; Except maybe for how list structured memory works. All the tail recursion came from eval
; being very careful saving only what it needs. Sometimes it pays to worry about
; efficiency.

; I hope you can believe someone can hold a Lisp evaluator in the palm of a hand.
; *chip in hand*, more complicated than the evaluator I just show you.
; 389 instructions of microcode. A total of 89.000 transistors.

