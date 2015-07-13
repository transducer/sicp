#lang scheme

;; Pattern-matching and Rule-based Substitution

; Well, yesterday we learned about symbolic manipulation and we wrote
; a rather stylized program to implement a pile of calculus rules.
; What we did is translate the rules into the language of the computer.
; Why should we have to translate the rules to the language of the computer?

; If it is this do that, if it is that do this, a dispatch.
; Well since it has such a stylized behaviour and structure, is there
; another way to write the same program?

; What are rules? Rules have parts. The rule has a left hand side and a
; right hand side. The left hand side side is compared with the expression
; you want to take the derivative of. The right hand side is the replacement
; for that representation.

; All derivation rules are patterns that are matched to a skeleton by a
; rule.

;          Rule
; Pattern -----> Skeleton
;   |               |
;   |               | Instantiation
;   v               |
; Expression | -> Expression
; Source          Target

; A pattern matches, a skeleton is something you substitute into in order to
; get a new expression.
; What that means is that a pattern is matched against the source expression
; and the result of the application of the rule is to produce a new
; target expression by instantiation of the skeleton.
; That is a process by which these rules are described.

; Today we will build a language to directly apply the rules.
; Instead of bringing the rules to the computer by writing a program that
; is those rules in the computer's language, we are going to bring the
; computer to the level of us by providing a way in which a computer can
; understand rules of this sort.
; Slightly emphasizing the idea that we are designing a solution to a class
; of problems instead of a particular one.
; The problem is that I want to write a rule for a different piece of
; mathemathics I'd have to write a different program, while I want to
; encapsulate all the things that are common to both of these programs 
; matching, instantiating, et cetera, the control structure, to encapsulate
; that separately from the rules themselves.

; Representation of the rules of calculus in a simple language. Where we
; avoid worrying about syntax or type-setting (this ain't TeX).

(define deriv-rules
  '(
    ( (dd (?c c) (? v))              0 )
    ( (dd (?v v) (? v))              1 )
    ( (dd (?v u) (? v))              0 )
    
    ( (dd (* (? x1) (? x2)) (? v))
      (* (dd (: x1) (: v))
         (dd (: x2) (: v)))            )
    
    ( (dd (* (? x1) (? x2)) (? v))
      (+ (* (: x1) (dd (: x2) (: v)))
         (* (dd (: x1) (: v)) (: x2))) )
    
    ( (dd (** (? x) (?c n)) (? v))
      (* (* (: n)
            (** (: x) (: (- n 1))))
         (dd (: x) (: v)))             )
    ))
    
; In the language we are inventing the question marks are pattern variables.
; The structure of the rules is that there is a left hand side that
; represents the expression and the right hand side the skeleton.
    
; Pattern matching
; foo     - matches exactly foo
; (f a b) - matches a list who's first element is f,
;                          who's second element is a
;                          who's third element is b
; (? x)   - matches anything, call it x
; (x x)   - matches a constant, call it x
; (?v x)  - matches a variable, call it x

; Skeletongs (instantiating)
; foo     - instantiates itself
; (f a b) - instantiates to a 3-list result of instantiating
;                          each of f, a, b.
; (:x)    - instantiates to the value of x in the pattern matched.     
            
; The most powerful design thing to do is making up a language to solve
; problems like this.

; We now have a list of riles called deriv-rules. What can we do with this?
; We are going to write a simplifyer.

(define dsimp
  (simplifier deriv-rules))

; (dsimp '(dd (+ x y) x))
; => (+ 1 0)

; Here are some other rules. Algebraic manipulation rules for simpliyfing:

(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                
      (: (op c1 c2))                )
    ( ((? op) (?  e ) (?c c ))                
      ((: op) (: c) (: e))          )
    ( (+ 0 (? e))                             
      (: e)                         )
    ( (* 1 (? e))                             
      (: e)                         )
    ( (* 0 (? e))                            
      0                             )
    ( (* (?c c1) (* (?c c2) (? e )))         
      (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          
      (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            
      (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          
      (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          
      (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            
      (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) 
         (* (?c c2) (? e))) 
      (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) 
         (+ (? e2) (? e3)))           
      (+ (* (: e1) (: e2)))         )
    ))

; What exactly these rules are does not really interest me. We want to
; simplify them.

; Each rule has a pattern and a skeleton. What I have is a matcher and an
; instantiator. I am going to pass from the matcher to the instantiator
; a dictionary of matched parts, then the instantiator will instantiate them
; and sent them to the matcher. I am going to feed the patterns to the
; matcher and the skeleton's to the instantiator.

; You stick an expression into this mess and after a while you take it out
; and it has been simplified. It keeps changing till it no longer can be
; changed.