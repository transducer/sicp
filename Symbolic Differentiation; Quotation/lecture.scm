#lang scheme

;; SYMBOLIC DIFFERENTATION; QUOTATION

;; Key idea of building robust systems it has to be insensitive to small
;; changes: a small change in the problem should lead to only a small change
;; in the solution.

;; How to do that: producing languages at different levels of detail where
;; subset of problems are solvable.

;; Then when you make changes you only have to make small changes to the
;; solution you have constructed because at the level of detail there is a
;; language you can express various solutions to alternate problems of the
;; same type.

;; That is the beginning of a very important idea. The most important idea
;; perhaps that makes computer science more powerful than most of the other
;; kind of engineering disciplines we know about.

;; What we have seen so far is the power of embedding languages in
;; procedures like the one below:

(define derivative
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define dx 0.001)

;; What we see here is a derivative program: a procedure that takes a 
;; procedure as an argument and produces a procedure as a value.

;; Now I am really going to muddy the waters. You see, such a 
;; procedure confuses the issue on what is a procedure and what is data,
;; but not very badly. What we really want to do is confuse it very badly...

;; And the best way to do that is to get involved with the manipulation of
;; the algebraic expression that the procedures themselves are expressed in.

;; So at this point I want to talk about rules in a calculus book.
;; Things like: the derivative of a constant is 0, et cetera.
;; These are EXACT expressions, these are not numeric approximations as the
;; derivative procedure above.

;; Can we make programs that manipulate these expressions?

;; Let's look at the derivative rules in some detail. And you know from 
;; calculus that it is easy to create derivatives. You also know that it is
;; difficult to produce integrals. Yet, integrals and derivates are inverse
;; operations. And they have the same rules. What is special about these
;; rules that makes it possible for one to produce derivates easily and
;; makes it so hard to produce integrals?

;; The expressions on the right hand side in the rule are subexpressions
;; of the expression of the left hand side.

;; See screen.

;; The other way around it is more difficult:
;; First of all there is more than one rule which matches. Which one to
;; take?
;; Also, the expressions become larger in that direction. Then there is no
;; guarantee that a particular path will terminate. Since it will only
;; terminate by accidental cancellation.
;; That's why integrals are complicated searches and hard to do.

;; Let's see if we can write a program which is these rules:

;; This is easy, just write the program. And we have a case analysis.

;; You have to believe I can represent these algebraic structures. We have
;; invented list structures so I can do that, but you don't want to worry
;; about that now. Right now I am going to write a program that contains
;; these rules, independent of the algebraic expressions.

(define (deriv exp var)         ; derivate of an expression with respect to
  (cond ((constant? exp var) 0) ; a variable. Here follows a case analysis.
        ((same-var? exp var) 1) ; A dispatch of the type of the expression.
        ((sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var)))
        ((product? exp)         ; predicate procedure end with question mark
         (make-sum              ; Lisp does not care. Humans care.
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (deriv (m1 exp) var)
                        (m2 exp))))
        ;; CONTINUE WITH COMPLETE SET OF RULES IN CALCULUS BOOK
        ))

;; So this is what it takes to encapsulate those rules. And you have to
;; realize there is a lot of wishful thinking here. I haven't told you
;; anything about how I am going to make these representations. Now once I 
;; have decided this is my set of rules, I think it is time to play with the
;; representation.

;; First of all I am going to play a pun. It's an important pun.
;; If I want to represent sum and products and differences and quotients and
;; things like that, why not use the same language as I am writing my 
;; program in?

;; I write my program in algebraic expressions that look like:
;; the sum of the product of a and the procuct of x and x and b times x
;; plus c:

;; (+ (* a (* x x))
;;    (+ (* b x)
;;       c))

;; What's nice of these lists is that I know where the car is. They are the
;; operators. And the operands are the successive cars of the cdrs of the
;; rest of this list.

;; This makes it very convenient: I don't have to parse it. It is done for
;; me. I am using the embedding in Lisp to advantage.

;; For example, let's start using Lisp structure to write down the 
;; representation I am implicitly assuming in the derivative procedure:

;; Constant means I can't break it up:
(define (constant? exp var) 
  (and (atom? exp) ; does not have a car and a cdr: it is not a list
       (not (eq? exp var))))

(define (atom? x) (not (or (pair? x) (null? x))))

;; Variables are the same if it is an atom and equal.
(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

;; A sum is something which is not atomic and begins with the plus symbol
(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+))) ; <= quotation means the SYMBOL

;; Now say YOUR NAME:
;; Girl in first row: "Susanna"
;; Guy next: "Your name"

;; The words in English are ambiguous. This cannot be distinguished in
;; speech. In writing we can distinguish them with quotations to 
;; distinguish the meaning. The same is true for Lisp.
;; The quotation means is it the symbolic +, which represents the addition 
;; operator.

;; Quotation is a very difficult subject, and adding it to a language causes
;; a great deal of trouble.
;; Consider the following text with a deduction we all agree with:

;;      Alyssa is smart.
;; Alyssa is George's mother.
;; --------------------------
;;  George's mother is smart.

;; "is" is an equality. We can always substitute equals for equals in
;; expressions. Or can we?

;;       "Chicago" has seven letters.
;; Chicago is the biggest city in Illinois.
;; ----------------------------------------
;;       "The biggest city in Illinois"
;;             has seven letters

;; Once we have things like that, our language becomes much more complicated.
;; Since it is no longer true, that things we like to do with languages
;; like substituting equals for equals and getting right answers, are going
;; to work without being very careful.

;; We can't substitute into referential opague contexts.
;; Of which a quotation is the prototypical type.

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define a1 cadr) ;; the car of the cdr of something

;; You might wonder about the names car and cdr, and why we do not use
;; things like left or right.
;; Well, the names come from the great past. In the time when Lisp was
;; invented. I assume in 1958 or so, there were machines that had an address
;; register and a decriment register. These are the contents of the 
;; address register and decriment register. So that's an historical accident.

;; Now why have they survived? It is because Lisp programmers like to talk
;; with each other over the phone. As you have a long sequence of cars and
;; cdrs you might say cddaadr, which can be understood. But right right left 
;; left right is not so clear.

;; All up to four deep are typically defined in a Lisp system. 

(define a2 caddr)

;; Example:
;; (+ 3 5)

;; List containing symbol + and symbol 3 and 5
;; The car is the symbol +, the car of the cdr is the first argument
;; and the car of the cdr of the cdr gives me the second argument.

;; Similarly I can define what is going on with products:

(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(define (make-product m1 m2)
  (list '* m1 m2))

(define m1 cadr)
(define m2 caddr)

;; Now, we have a complete program for finding derivatives. 
;; You can add more rules if you like.
;; What kind of behaviour do we get out of it?

;; Well,

(define foo ; a*x*x + b*x + c
  '(+ (* a (* x x))
      (+ (* b x)
         c)))

(deriv foo 'x) ; Expected is 2*a*x + b
;; => (+ (+ (* a (+ (* x 1) (* 1 x))) 
;;         (* 0 (* x x))) 
;;      (+ (+ (* b 1) (* 0 x)) 
;;         0))

;; We get this horrible mess. I would like it to be 2ax + b.
;; But it's not. It is equivalent to it, but I would have been taken up
;; points on an exam for that.

;; Let's look at improving it in the next segment.

;; We just developed a fairly convincing program for finding the
;; deriviates of algebraic expressions. For a complete program we like
;; more rules or the possibility to add more arguments to multiplication
;; or addition, that is all rather easy.

;; However, there is a little issue with the expression that we get.
;; They are rather bad. Why do we get such a large expression?

;; Let's look at the pieces and find where they all come from.

;; => (+ (+ (* a (+ (* x 1) (* 1 x)))
;;          (* 0 (* x x))) 
;;       (+ (+ (* b 1) (* 0 x)) 
;;          0))

;; First we have a sum of x times 1 plus 1 times x,
;; that is the derivative of the product.
;; The part of a times that is the sum around it. It is the first thing
;; times the derivative of the second plus the derivative of the first
;; times the second thing. As the program we wrote indicated we should do.

;; And of course the product of b x manifests itself as b times 1 plus
;; 0 times x, because we see that b does not depend on x, so the derivative
;; of b is 0 and the derivative of x with respect to itself is the 1.

;; And of course the derivates of the sum turn into the two parts of the
;; derivatives of the parts. 

;; What we are seeing here is that the form of the process is expanded
;; from the local rules we see in the procedure.

;; Here the process left behind some stuff which was the answer. It is 
;; constructed from the tree structure which is the expression.
;; So every part in the answer derives from some part of the problem.

;; Now, we can look at the derivative of foo, which is a*x*x + b*x + c, 
;; with respect to other things. Like with respect to 'a, 'b or 'c

(deriv foo 'a)
;; => (+ (+ (* a (+ (* x 0) (* 0 x))) 
;;          (* 1 (* x x))) 
;;       (+ (+ (* b 0) (* 0 x)) 
;;           0))

(deriv foo 'b)
;; => (+ (+ (* a (+ (* x 0) (* 0 x))) 
;;          (* 0 (* x x))) 
;;       (+ (+ (* b 0) (* 1 x)) 
;;          0))

(deriv foo 'c)
;; => (+ (+ (* a (+ (* x 0) (* 0 x))) 
;;          (* 0 (* x x))) 
;;       (+ (+ (* b 0) (* 0 x)) 
;;          1))

;; Bit different answers, but shapes of the expressions are the same.

;; Is there anything wrong with our rules? No.

;; There aren't too many good ideas. 

;; When we were looking at rational numbers a few days ago, we got 6/8
;; instead of 3/4. The answer was unsimplified. The problem now is very
;; similar. There are things I like to be identical by simplification that
;; don't become identical. And yet the rules for doing multiplication and
;; addition for rational numbers were correct. So the way we might solve 
;; this problem might be the same as last time. If something worked last 
;; time it ought to work again.

;; We change the representation. Perhaps in the representation we can
;; put in a simplification step. That produces a simplified representation.
;; This may not always work of course, I am not trying to say it always
;; works. But it is one of the pieces of artillary we have in our war
;; against complexity.

;; You see because we solved our problem very carefully, what we did was
;; we divided our world into several parts:
;; - There are derivative rules
;; - And general rules for algebra of some sort
;; - And an abstraction barrier
;; - And the representation of the algebraic expressions (list structure).

;; In the barrier we have the interface procedures like constant?, same-var?,
;; sum?, make-sum, a1, a2. Making that barrier allows me to arbitrarily
;; change the representation, without changing the rules I have written in
;; terms of that representation.

;; So if I can make the problem go away by changing the representation
;; decompositioning the problem into the two parts have helped us a great
;; deal.

;; Let's go back to the results. Why is adding 0, multiplying by 1 and
;; the product of 0 included in the result? We can most surely make the
;; representation smarter than that.

;; We don't have to make the big construction. Let's do that by changing the
;; way the representation works.

(define (make-sum2 a1 a2)
  (cond ((and (number? a1)
              (number? a2)) ; they are not symbolic expressions, then do the
         (+ a1 a2))         ; addition straight away.
        ((and (number? a1) (eq? a1 0)) ;; then the answer is just a2,
         a2)                ; there is no reason to make anything up.
        ((and (number? a2) (eq? a2 0)) 
         a1)                ; Only if I can't figure out anything better
        (else (list '+ a1 a2)))) ; I construct a list.

;; Similarly, make-product
(define (make-product2 m1 m2)
  (cond ((or (eq? m1 0) (eq? m2 0)) 0)
        ((eq? m1 1) m2)
        ((eq? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv2 exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum2 (deriv2 (a1 exp) var)
                    (deriv2 (a2 exp) var)))
        ((product? exp)
         (make-sum2
          (make-product2 (m1 exp)
                         (deriv2 (m2 exp) var))
          (make-product2 (deriv2 (m1 exp) var)
                         (m2 exp))))
        ;; CONTINUE WITH COMPLETE SET OF RULES IN CALCULUS BOOK
        ))

(deriv2 foo 'x)
;; => (+ (* a (+ x x)) b)
(deriv2 foo 'a)
;; => (* x x)
(deriv2 foo 'b)
;; => x
(deriv2 foo 'c)
;; => 1

;; By only changing the constructor the same foo is now producing
;; simplified results. It is still not completely simplified, but doing
;; that takes a lot of code. Simplifying algebraic expressions is hard.

;; What we seen is a carefully organised example of how we can manipulate
;; algebraic expressions, how we can do that abstractly using abstract
;; syntax rather than concrete syntax, and how we can use the abstraction
;; to control what goes on in building these expressions.

;; But the real story isn't such a simple thing as that.
;; The real story is in fact that I am manipulating these expressions
;; and the expressions are the same expressions as the ones that are Lisp
;; expressions.

;; There is a pun here. I have chosen my representations to be the same as
;; the representation in my language. Of similar things. By doing so I
;; invoked a necessity. I created a necessity to have things like quotation.
;; Because of the fact my language is capable of writing expressions that
;; talk about expressions of the language. I need to have something that 
;; says "this (e.g., '+) is something of an expression I am talking about,
;; rather than this expression is talking about something and I want to talk
;; about that".

;; So quotation says "stop, I'm talking about this expression itself".

;; Now, given the power that I can manipulate expressions of the language I
;; can begin building even more powerful layer upon layer of languages.
;; Cause I can write languages that not only are embedded in Lisp (or
;; whatever language you start with), but languages that are completely
;; different, that are just as we say, "interpreted in Lisp" or something
;; like that.

;; We'll get to understand those words more in the future, but right now I
;; just want to leave with you with the fact that we've gotten over a line
;; which gives us tremendous power. At this point we bought a sledgehammer
;; and we have to be careful on what flies apply it.