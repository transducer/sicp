#lang scheme

; We now take a look at the matcher. It is a box that takes as his input
; an expression and a pattern, and a dictionary (mapping of pattern
; variables to the values that were found by matching) and it puts out
; another dictionary which is the result of augmenting the input dictionary
; with what was found in matching the expression with the pattern.

; This is a rather complicated program, and we can look at it and see
; see it is very complicated. It is a case analysis. What we're now going
; to do is look at it in a bit more detail. Here's some of the structure:

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         ; *** atomic patterns
         )
        ; *** Pattern variable clauses
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
                (cdr exp)
                (match (car pat)
                       (car exp)
                       dict)))))

; We have to compare two trees simultaneously. One is the tree of the
; expression and one of the pattern. We have to compare them with each
; other. So that the subexpressions of the expression are matched against
; subexpressions of the pattern.

; Suppose I have a pattern that is the sum of the product who's second
; argument is the same as the second argument of the sum:

(+ (* (?x) (?y)) (?y))

; As a pattern it looks like a tree which consists of a sum, and a product
; with a pattern variable question mark x and y, and a question mark y.

; If we are matching this against a pattern expression which matches it:

(+ (* 3 x) x)

; It is the sum of the product of 3 and x and x.
; What i want to do is traverse these two trees simultaneously.
; What I'd like to do is walk them from left to right.
; I put in my dictionary equals 3 and x equals x.
; And the pattern variable y matches x. Yep done.
; I now have a dictionary which I accumulated by making this walk.

; Now let's look at the general case and see how it works.

; I am matching the cars of the pattern and expression with respect
; to the dictionary I already have. Producing a dictionary as its value
; which I will then use for matching the cdrs against each other.
; That is how the dictionary travels over the whole pattern. And then the
; result of that is the dictionary for the match of the car and the cdr.
; And that is what is going to be returned as a value.

; In any point a match might fail. This may be the case for example, if we
; look at an expression that does not quite match syntactically.
; When a match fails and it takes the dictionary from the previous match
; as input it must be able to propagate the failures. That is what the first
; clause of the conditional does. 

; It it also true that if the pattern is not atomic and the expression is
; atomic (made out of pieces) then that must be a failure.

; If it is atomic this is what we see:

((atom? pat)
 (if (atom? exp)
     (if (eq? pat exp)
         dict
         'failed)
     'failed))

; If the pattern is atomic, then if the expression is atomic, then if they
; are the same thing then the dictionary I get is the same one as I had
; before. Nothing has changed. I matched for example + against + 
; or * against *. However, when the pattern is not the one which is the 
; expression. When I have two separate atomic objects it fails (for example
; when matching + against *). Or if it turns out the pattern is atomic
; but the expression is complicated (in Lisp this means consisting of two
; things) then I also get a failure.

; Now what about the various kind of pattern variables?:

; A question mark x is an arbitrary expression. A question mark c x is an
; arbitrary constant. And a question mark z x is an arbitrary variable.

((arbitrary-constant? pat)
 (if (constant? exp)
     (extend-dict pat exp dict)
     'failed))

; If the pattern is an arbitrary constant it better be the case that the
; expression is an arbitrary constant. If the expression is not a constant 
; then that match fails. If it is a constant however I wish to extend the 
; dictionary with that pattern being remembered being that expression, using
; the old dictionary as a starting point.

((arbitrary-variable? pat)
 (if (variable? exp)
     (extend-dict pat exp dict)
     'failed))

; For arbitrary variables as pattern I have to check first if the 
; expression is a variable. If so I extend the dictionary so that the
; pattern is matched to be that expression given the original dictionary.
; This makes a new dictionary.
; Now it has to check within this dictionary that if one of the pattern
; variables already has a value. I am trying to match the thing against
; something else which is not equivalent to the thing I already matched
; against then a failure will come flying out of extend-dict too. We will
; see that.

((arbitrary-expression? pat)
 (extend-dict pat exp dict))

; Finally, an arbitrary expression does not have to check anything
; syntactically against the expression that is being matched. So all 
; what it does is an extension of the dictionary.

; What is somewhat remarkable these days, is that people pay an awful lot
; of money for someone to make an "AI expert system" that has nothing more
; in it than a matcher and maybe an instantiator like this. 
; But it is very easy to do, and now of course you can start a little
; startup company and make a couple of megabucks.

; Now there is an instantiator as well:
; The goal of the instantiator is to make an expression given a dictionary
; and a skeleton:

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

; We are going to do a recursive tree walk of the skeleton, and for 
; everything that is a skeleton variable, I don't know call it a
; skeleton evaluation, a thing beginning with a colon in the rules.
; For anything like that I am going to look up the answer in the dictionary.
; We will worry about that in a second.

; We are instantiating a skeleton given a dictionary. We are defining an
; internal loop which is doing the following:
; Either the skeleton is simple and atomic, in which case it is nothing more
; then giving the skeleton back as an answer. 
; Or in the general case it is complicated, in which case I am going to make
; up the expression which is the result of instantiating (calling the loop
; recursively) the car of the skeleton and the cdr of the skeleton.
; (This is a recursive tree walk.)
; However, when it turns out to be a skeleton evaluation (a colon expression
; in the skeleton), then what I am going to do is find the expression that
; is in the colon and evaluate that relative to the dictionary (whatever
; evaluation means, we will find out that sometime) and the result of that
; is my answer.
; I start of the loop by calling it with the whole skeleton. This will just
; do a recursive decomposition into pieces.

; Now, one more little bit of detail. What happens inside of evaluate. I
; can't tell you that in great detail. I will tell you a little bit of it,
; and later we will look into this in much more detail:

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (mapcar (lambda (v)
                 (lookup v dict))
               (cdr form)))))

; To evaluate a form with respect to a dictionary.
; If the form is atomic I will look it up. 
; Otherwise, I am going to do something complicated: which is applying
; a procedure which is the result of looking up the operator part in 
; something (user-initial-environment) we will learn about some day. I want
; you to realize we are looking at magic now. This magic will become clear
; very soon, but not today.
; And then looking up all the pieces and all the arguments to that in the
; dictionary.
; Don't look at this in detail. We will see more in the future. The magic
; is going to stop. This part has to do with Lisp.

; Now we know about matching and instantiation. Are there any questions?

; The idea is to pass failed back to the dictionary, is that right?
; => The dictionary is the answer to a match. Right? And it is either some
;    mapping, or there is no match. So what you are seeing is that because 
;    of the fact that the car can fail, the match of the cdr needs to
;    propogate that fail. That is what the first line does.
; I am still unclear what comes out of one instance of match?
; => One of two possibilities: either the symbol failed, which means there
;    is no match, or some mapping which is an abstract thing right now (you
;    should know about the structure of it) which relates the pattern
;    variables to their values as picked up in the match.
; The recursive nature brings about the fact that if ever a fail gets passed
; out of any calling of match then the first condition will pick it up ...
; => ... and the first condition will pass it along without any further ado.
; ... Alright okay.
; => That is just the fastest way to get that failure out of there.
; If I don't fail that means I have matched the pattern and I run the
; procedure extend-dict and then pass the result into the expression, but
; the substitution will not be made at that time, is that right?
; => There is no skeleton there so nothing is substituted. All we are doing
;    is making up the dictionary for a later substitution.
; What would the dictionary look like.
; => That is not told to you. That is abstract. What you want to know is
;    that it is a function. A function abstractly is a set of ordered pairs.
;    It could be implemented as a set of Lisp pairs, or a fancy table
;    mechanism, it could be implemented as a function. But I am not telling 
;    you. That is up to George who is going to build that up later.
; Let me at least know what is the important information that is passed to
; extend-dict? I want to pass the pattern I found ...
; => Yes, the pattern that is matched against the expression. You want to
;    have the pattern which happens to be in those cases pattern variables.
;    All of those three cases for extend-dict are pattern variables. So you
;    have a pattern variable that is going to be given a value in a
;    dictionary. The value is the expression that it is matched against.
;    The dictionary is the set of things I have already figured out, that
;    I have memorized or learned. And I am going to make a new dictionary
;    which is extended from the dictionary I already had by having that 
;    pattern variable have a value within the new dictionary.
; Why can't the substitution not be made right away?
; => How do I know I am going to substitute? I don't think about this
;    skeleton. This matcher is an independent unit.
;    I take the matcher, I apply the matcher, if it matches then I do
;    the instantiation.
; Can you do that answer again using that example on the board?
; => When I am traversing the structure I get to a leave, x, I have an empty
;    dictionary presuming this is the whole expression and I have matched x
;    against 3. So now after this point the dictionary contains x is 3. I
;    continue walking and I see y. Now this is a particular x, a pattern y.
;    The dictionary now says "o yeah, the pattern y is the symbol x". Cause
;    I've got a match there. So the dictionary at this point contains two
;    entries: the pattern x is 3 and the pattern y is the expression x.
;    Now I can walk along further and I see "oh, pattern y also wants to be
;    4. That is not possible, producing a failure.