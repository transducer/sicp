; Let's see. At this point you should be getting the feeling: "What is this
; nonsense this Sussman character is feeding me?" There is an awful lot of 
; strange nonsense here. After all, he wanted to explain to me Lisp and
; wrote a Lisp program on the blackboard. The Lisp program was intented to
; be an interpreter for Lisp, but you need a Lisp interpreter to understand
; that program.

; How could that program told me anything there is to be known about Lisp? 
; How is that not completely vacuous?

; The whole thing is like Escher's hand drawing each other. Eval and apply
; draw each other and construct the real thing that can sit down and draw
; itself.

; What I am going to do know is convince that this means something and as
; an aside explain why you don't need definitions for doing all we need
; to do for computing.

; = > FIXED POINT explanations (apply function again and again leads to
;                               fixed point in cases that we want)

; expt = lim Em when n goes to infinity

; expt (f (f (f (f ... (f anything) ...))))
; infinite nesting of f's lead to a fixed point (in the cases we are
; interested in)

(Y f)  = (f (Y f))

Y = (lambda (f)
      ((lambda (x) (f (x x)))
       (lambda (x) (f (x x)))))

; Let's think about this Y-combinator

; Lisp is the fixed point of the process that says if I knew what Lisp was
; and substituted it in for eval and apply on the right hand side of all
; those recursion equation, then if it was a real Lisp the left hand side
; would also be Lisp.

; These limit arguments are quite dangerous and don't always work. It is a
; very difficult field of study and hard to prove. 

; We have been granted into 
; the grand recursive order of lambda calculus hackers.
