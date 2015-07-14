#lang scheme

; You have just seen your first big and hairy program. One of the goals of
; this class is to be able to read something like that and not be afraid of
; it. This one is only about four pages of code. By the end of the subject
; I hope a fifty page program will not look particulary frightening.
; What I don't expect, and I don't want you to think that, is that you will 
; be getting it when it comes out. There is a lot of stuff inside the 
; program and I want you to have it slowly sink in. 

; Now I've talked about the language we are implementing (pattern matching
; substitution language), and I've told you about matching and instantiation
; which are the two halves. Now I want to talk about the control structure
; in which the rules are applied to the expressions, so as to do algebraic
; simplification.

; That is also a big complicated mess. The problem is there is a variety of 
; interwoven loops. For one thing, I have to examine every subexpression of 
; the expression I am trying to simplify. That is what I have to do, it is
; a car cdr recursion of some sort. Some sort of tree walk. For every node
; that I get to in doing my traversal of the expression I am trying to 
; simplify, I am trying to apply all of the rules. Every rule is going to
; look at every node. I am going to rotate the rules around. Now, either 
; rule will or will not match. If the rule does not match then it is not
; very interesting. If the rule does match then I am going to replace that
; node in the expression by an alternate expression. I will make up a new
; expression that contains everything that contains that new value as the 
; result of instantiating the skeleton of that rule at that level. But no
; one knows if that things I have instantiated there is its simplified
; form. So I have to simplify that. Somehow I have to call the simplifier
; on the thing I have just constructed. And when then that is done I can 
; build that in the expression that is my answer.

; Now, there is a basic idea here which I will call a garbage in garbage out
; (GIGO) simplifier. Simple objects are simple, compound objects, well, I
; don't know. What I am going to do is building up from symbol objects
; trying to make simple things by assuming the pieces they are made of
; are simple.

; Now, look at this very complicated program. It has various subprograms,
; a part for traversing an expression and one part for trying rules. 
; We will look at it in some more detail.

; The simplifier is made out of several parts:

(define (simplifier the-rules)
  define (simplify-exp exp)
  ;***
  )
(define (simplify-parts exp)
  ;***
  )
(define (try-rules exp)
  ;***
  )
simplify-exp)

; Now remember the simplifier is a thing that takes a set of rules and 
; produces a program which will simplify relative to them.
; Our simplifier takes a rule set. There are various other definitions done,
; and then the result is one of the procedures that is defined.
; What I return as the value of calling simplifier on a set of rules,
; is a procedure. The simplify-exp procedure which is defined in that
; context. Which is a simplification procedure appropriate for using those
; set of rules.

; That is what I have there. The first two procedures are going to be the
; recursive traversal of an expression. The above one is the general
; traversal of a pattern (simplify-exp), and the second one will traverse 
; the list of parts of an expression (simplify-parts). For each of those we 
; will do something complicated which is trying the rules (try-rules).

; Look at the recursive traversal of the expression. A nest of recursive
; procedures:

(define (simplify-exp exp)
  (try-rules (if (compound? exp)
                 (simplify-parts exp)
                 exp)))

(define (simplify-parts exp)
  (if (null? exp)
      '()
      (cons (simplify-exp (car exp))
            (simplify-parts (cdr exp)))))

; If the expression I am trying to simplify is a compound expression I am
; going to simplify all the parts of it. That procedure simplify-parts is
; going to make a new expression with all the parts simplified on which
; I am then going to try the rules on. If it turns out the expression is 
; not compound (e.g., PI) then I am going to try the rules on them (and it
; turns out PI evaluates to 3.14......).

; If I want to simplify the parts and it is null I just have an empty 
; expression, otherwise I will create a new expression by cons which is the
; result of simplifying the first part of the expression (car) and 
; simplifying the rest of the expression (cdr).

; Now the reason why I am showing it in this way is because I want you to
; get a feeling of the various patterns which are very important when 
; writing programs.
; There is another way to write simplify-exp so there is only one of them:
; (Another idiom, if you will)

(define (simplify-exp exp)
  (try-rules
   (if (compound? exp)
       (map simplify-exp exp)
       exp)))

; Now you don't need the helper procedure simplify-parts since it is really
; map. You can also write it that way, it does not matter very much.

; Now let's look at try-rules:

(define (try-rules exp)
  (define (scan rules)
    ;***
    )
  (scan the-rules))

; This is a complicated mess also. When I am trying rules on an expression
; it turns out the expression I am trying is some subexpression now of the
; expression I started with. Because the thing I just arranged allowed us
; to try every subexpression. 

; Here we take in a subexpression and we are defining a procedure called 
; scan which will try all rules and cdr down them, looking for a rule to
; apply, and when it finds one it will do the job. Let's take a look at
; how scan rules work:

(define (scan rules)
  (if (null? rules)
      exp
      (let ((dict
             (match (pattern (car rules))
               exp
               (empty-dictionary))))
        (if (eq? dict 'failed)
            (scan (cdr rules))
            (simplify-exp
             (instantiate
                 (skeleton (car rules))
               dict))))))

; We take a bunch of rules which is a sublist of the rules we already tried.
; If there are no more rules there is nothing I can do with this expression
; and it is simplified. However, if it turns out there are still rules to
; be done then let's match the pattern of the first rule against the
; expression using the empty dictionary to start with, and use that as a
; dictionary.
; If that happens to be a failure, try the rest of the rules.
; Discard that rule (cdr), otherwise I am going to get the skeleton of the
; first rule and instantiate that relative to the dictionary and simplify
; the result, and that is the expression I want.

; Although that was a complicated program it was made up out of simple
; pieces. Now the pattern of recursion is very complicated. And one of the
; most important things is not to think about that. If you try to think
; about the actual pattern by which this does something, you are going to
; get very confused. I would. This is not a matter of that you can do this
; with practice. These patterns are hard. But you don't have to think 
; about it. "The key to very good programming and very good design is to
; know what not to think about." The fact is I don't have to think about
; it. I have expectations in my mind about what simplify-exp does. I don't
; have to know how it does it. And it may in fact scall scan somehow to
; try-rules (which it does) which has some more recursion going on.
; But I can use wishful thinking and assume that it will produce the
; simplified result and I don't have to think about it anymore.

; There is a very little left, only what is left is a few details on what
; a dictionary. Well, a dictionary is represented in something which is 
; called a list which is a particular pattern of usage for making tables
; in Lisp. They are made out of pairs as was asked. There are special 
; procedures for dealing such things called assq. You can find them in
; manuals. I am not terribly excited by it. The only interesting thing here
; is that I will have to extend a dictionary with a pattern, a datum and
; a dictionary.

(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v)
             (cond (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))

; I want to pull out the name of the pattern variable name (pat) and I am
; going to look it up in the dictionary and see if it already has a value.
; If not, i am going to add a new one in. If it does have a value then it 
; better be equal to the one that was already stored away. And if that is
; the case the dictionary is what I expect it to be. Otherwise failed.

; If you open up any program you are going to find inside lots of little
; pieces. All of which are easy.

; At this point I told you some million dollar valuable information and it 
; is time for questions:

; Can you give me the name for using simplify-exp?
; => Sure, simplify-exp takes an expression and produces a simplified
;    expression. That's it. How it does it is very easy. It checks if there
;    is a compound expression and if so splits it up and then all the rules 
;    are tried on the result, and when I have a simple expression I just try
;    all the rules.
; The expression can be simplified by virtues of the rules.
; => That is of course true. It breaks the expression up into the smallest
;    pieces using the rules and constructs a new expression using the
;    result. Furthermore, try-rules will call simplify-exp when it changes
;    something (the result of an instantiation on the skeleton for a 
;    result that has matched).