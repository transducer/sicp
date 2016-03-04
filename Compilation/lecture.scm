; Lecture PART 1

; The explicit control evaluator bridged the gap betewen high-level languages like Lisp,
; the query language and all that stuff, and a conventional register machine.

; You can think of the explicit control evaluator as the code for a Lisp interpreter if you
; want to implement it on a conventional register transfer machine if you want to implement
; it in assembly language. Or think of it as the microcode of someone that's going to be
; specially designed to run Lisp.

; We raise a machine that speaks a low level language and we raise it to a high level
; language by writing an interpreter.

; An interpreter configures itself to emulate a machine of which a description you read in.

; Register language interpreter can configure itself to behave like a Lisp evaluator.
; And it needs a library for primitive interpretation and external stuff.

; There is a different strategy than interpretation to raise a machine's language to a higher
; one. That is COMPILATION.

; Take source code to compiler which translates it into register machine language and that
; will make a register language for computing factorials (not an evaluator for Lisp).
; That will go into some sort of loader which combines it with code selected from the
; library to do things like primitive multiplication. Then we'll procude a load module to
; configure the register language machine to be a special purpose [goal of source code]
; machine.

; So, in interpretation, we're raising the machine to the level or our language, like Lisp.
; In compilation, we're taking our program and lowering it to the language that's spoken
; by the machine.

; How do these compare? The compiler can produce code that can execute more efficiently.
; The essential reason for that is that if you think about the register operations that are
; running, the interpreter has to produce register operations which in principle have to
; be general enough to execute any Lisp procedure. Whereas the compiler only has to worry
; about producing a special bunch of register procedures for doing the particular Lisp
; procedure that you've compiled.

; The interpreter is a general purpose simulator whereas the compiler is in effect
; configuring the machine to be the interpreter that the machine would have been simulating.

; The compiler can be faster. But the interpreter is a nicer environment for debugging. The
; reason for that is that we've got the source code actually there.

; Most Lisp system end up being both. Use an interpreter for evaluating code and then you
; can speed it up by compiling. And often you can arrange that compiled code and interpreted
; code can call each other. We'll see how to do that.

; To do that we have the compiler use exactly the same register conventions as the
; interpreter. 

; An evaluator walks over the code and performs some register operations. A compiler walks
; over the code and produces the register operations that an evaluator would have done were
; it evaluating the thing.

; That gives us a model for implementing a zeroth-order compiler. But a very bad one.

Register Operations in interpreting (F X)

(assign unev (operands (fetch exp))) ;; Pull operands out of exp register and assign to unev
(assign exp (operator (fetch exp)))  ;; Assign something to exp register
(save continue)                      ;; save continue, et cetera 
(save env)                           ;; Write down the register assignments that the 
(save unev)                          ;; evaluator would have done in executing that code
(assign continue eval-args)
(assign val (lookup-var-val (fetch exp) (fetch..)))
(restore unev)
(restore env)
(assign fun (fetch val))
(save fun)
(assign argl '())
(save argl)
(.. altogether about 19 operations)

; This will be the code up until the point where the evaluator branches off to apply-dispatch.

; In the compiler we are not worrying about apply-dispatch. It stashes away the operations
; instead of actually executing them. Not quite true. One little lie in this. What you have
; to worry about is that if you have a predicate and some kind of test you want to do. At the
; point when you are compiling it you don't know which branch of a condtional you are going
; to do. You can't say which one the evaluator would have done. So all you do there is
; compiling both branches.

(if p a b) ; compiles into

  <code for p -- result in val>
  branch if val is true to label1
  <code for b>
  goto next thing ;; whatever was supposed to happen
label1 <code for a>
  goto next thing

; That is how you treat a conditonal. Other than that the zeroth-order compiler is the same
; as the evaluator. Stashing away the instructions instead of executing.

; Already this is more efficient. The evaluator is not only generating the register
; operations. But also deciding which ones to generate. If you run the evaluator a million
; times that evaluator phase is running a million times. Whereas in the compiler it's
; happened once and then you just have the register operations themselves.

; That's a zeroth-order compiler. It's really dumb.

; For example, we do not need unev or exp in the example since there are just X and F.
; From the compiler's point of view they are just constants.

; Similarly,
(assign continue eval-args)

; That was just the evaluator keeping track of where it should go next, to evaluate the
; arguments in some application. That's irrelevant to the compiler because the analysis
; phase will have already done that.

; If we simple get rid of the irrelevant registers and assignments to continue, then we can
; take the literal code of the evaluator and replace them with this:

(save env)                               ; <= this was needed by the evaluator because
(assign val                              ;    it was going to recursively call eval-dispatch
        (lookup-var-val 'f (fetch env))) ;    and it needed the environment later.
(restore env)                            ;    But the actual thing it was doing is not going
(assign fun (fetch val))                 ;    to hurt the environment at all. So no reason to
(save fun)                               ;    saving and restoring it.
(assign argl '())                        ; <= Similarly here, saving the argument list.
(save argl)
(assign val
        (lookup-var-val 'x (fetch env)))
(restore argl)                           ; <= And restoring it here. The actual thing didn't
(assign argl                             ;    modify it so no reason to save it.
        (cons (fetch val) (fetch argl)))
(restore fun)

; We have gotten rid of the irrelevant stuff and about half of it remains. We have put in
; the constant 'f and the constant 'x for example.

; This is a little better compiler but it's still pretty dumb.

; The evaluator has to be maximally pessimistic, because from its point of view it's just
; going off to evaluate something so it better save what it's going to need later.
; But once you've done the analysis the compiler is in the position to say: "Well, what
; actually did I need to save?" It doesn't need to be as careful.

; If we eliminate all those redundant save and restores, then we can get it down to this:

Eliminating unnecessary stack operations

(assign fun
        (lookup-var-val 'f (fetch env)))
(assign val
        (lookup-var-val 'r (fetch env)))
(assign argl (cons (fetch val) '()))

computation proceeds at apply-dispatch
 
; There are only three instructions left that we actually need, down from the initial ~20.

; That's the basic idea: take the evaluator and eliminate the things we don't need (that
; in a sense only have to do with the evaluator) and then you see which stack operations
; were unnecessary. That's the basic idea of the compiler described in the book.

; The interpreter does not know what it will encounter, the compiler only has to do with
; what actally had to be saved.

; Two reasons that things might not have to be aved:
; 1. What you're protecting it against in fact didn't trash the register (like if it is just
;    a variable look-up).
; 2. The thing that you were saving it for might turn out not to actually need it.

; Two basic reasons that the compiler can take advantage of in making the code more efficient.


; Lecture PART 2

; Now we will look at the essential idea of how this is accomplished. No code, the code is in
; the book.

(op a, a2) 

; Recursively go off and compile the op, and the result should end up in the function
; register. (This piece should be done preserving the env register.)

; Compile first argument a and put result in val. (Preserve fun register.)
(assign argl (cons (fetch val) '()))

; (Check the environment for the second argument and preserve env.)

; Compile a2 and put result in val.
; (Preserve argl.)
(assign argl (cons (fetch val) (fetch argl)))

(goto apply-dispatch)

; The basic means of combining things is to append to code sequences:
append seq1 and seq2 preserving reg

; And that's a bit tricky.

IF SEQ2 NEEDS REQ
AND SEQ1 MODIFIES REG

; Then the instructions the compiler spits out are:

(save reg)
<seq1> ; Put out the recursively compiled stuff for seq1
(restore req)
<seq2>

OTHERWISE
<seq1>
<seq2>

; That is the basic operations for sticking together the code segments.

; Difference between the interpreter and the compiler is that where the compiler has this
; preserving notes the interpreter always has a save and restore.

; The bottom level data structures look like this:

<sequence of instructions; set of registers modified; set of registers needed>

; For example:
<(assign r1 (fetch r2)) ; {r1}; {r2}>

; When it combines two sequences.
<s1; m1; n1> and <s2; m2; n2> 

; Then the new code fragment seq1 followed by seq2, what will it modify?

; The union of m1 and m2 is what it modifies. It needs n1 and the ones that are needed by n2
; that have not been modified by m1.

<s1 and s2; m1 v m2; n1 v [n2 - m1]>

; At the primitive level this is it. That is the whole thing except 30 pages of details.

; It is a good rudimentary compiler.

; It can be optimized further. One of the reasons FORTRAN is faster than Lisp is probably
; because of the effort that has gone into compiler optimizations.
