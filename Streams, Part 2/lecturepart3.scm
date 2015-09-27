#lang scheme

; Before the break something nasty started to happen with these streams
; and divorcing time from the programs. Sometimes to really take
; advantage of this method you have to write some explicit delays that
; are not hidden in that cons-stream. I did a simple example, but when you
; have a lot of nested loops it becomes very difficult to see where you
; need those delays.

; And if you leave them out by mistake it becomes difficult to see why the
; program is not working.

; So with streams you can get some very complicated programming sometimes 
; because it cannot all be hidden in the streams.

; Is there a way out? Yeah. We could change it so that every procedure acts
; like a cons-stream, so that every procedure has a delay around its
; arguments. Only evaluated when you need them. Passing promises around.

; If we did that everything has a uniform delay and you would not need
; explicit delays because it is automatically built in how the language
; works.

; If we did that our language would be NORMAL-ORDER EVALUATION instead of
; what we had so far which is APPLICATIVE-ORDER EVALUATION.

; Normal-order substitutes the arguments with the procedures but instead of
; evaluating them you just put a promise to compute them there.
; Applicative order does evaluate.

; Why wouldn't we do that? Why not make lists automatically streams?

; You never do computation until you actually need the answer.

; People do do that. You have some very beautiful languages (one of the
; nicest is called Miranda developed by David Turner).
; It works like that, data structures are streams and they do these things.

; There is a price in decoupling time for the program from time for the 
; machine.

; We like to think as programming as a way to specify processes. If we give
; up too much time our language becomes more elegant, but less expressive.
; There are certain distinction we cannot draw. 

; One of them is iteration.

(define (fact-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

; An internal procedure, with a state that is a product and a counter. It 
; is not building up state since the iterative procedure does not have
; to grow.

; In normal order language it would no longer be an iterative procedure.
; It would grow when iter calls itself with the product: in a normal order
; language this computation would not get called. It gets around again and
; calls itself with a promise where one of the factors is a product.
; A growth in state of all the promises that only get evaluated at the end.

; People who are trying to write operating systems in these languages are
; having exactly the same problems. It is perfectly possible to implement
; a text editor in languages like these. But after you work for a while
; you certainly have three megabytes of stuff which is the "dragging tail
; problem" - these promises that are not yet called because you could not
; express an iteration. 

; One of the research questions in these languages is figuring out the 
; right compiler technology to get rid of the dragging tails.

; There's another more striking issue in why you don't go ahead and make
; your language normal-order.

; The reason is: normal-order evaluation and side-effects just don't mix.

; You can't simultaneously model objects with local state and change, and
; at the same time do these normal-order tricks of decoupling time.

; Let me show you a simple example. Suppose we have a normal-order language.

(define x 0)            ; Define x to be 0

(define (id n)          ; Define a little funny function which is an
  (set! x n)            ; identity function that keeps track of the last
  n)                    ; time you called it using x.

(define (inc a) (1+ a)) ; And we have a little increment function.

; A simple scenario. Normal-order:

(define y (inc (id 3)))

x ---> ???    ; What is x? It is not 3. We have an increment as a promise to
              ; do something. You will get 0.
y ---> 4      ; The very act of looking at y causes the identity function to
              ; run.
x ---> ???    ; Here x is 3.

; You see what kind of a mess this is going to make for debugging
; interactive programs when you have normal-order evaluation. It is very
; confusing. But it is very confusing for a very deep reason, which is that
; the whole idea of putting in delays is that you throw away time. That's
; why we can have these infinite processes. If we don't have time we don't
; have to wait for them to run. We decouple the order of events in the
; computer from what we write in our programs. But when we talk about state
; and set! and change, that is exactly what we do want control of. So it is
; almost as there is this fundamental contradiction in what you want.

; This brings us to philosophical muttering on what is it that you are 
; trying to model and how do you look at the world. Or sometimes it is 
; called the debate of functional programming.

;    FUNCTIONAL PROGRAMMING

; A so-called purely functional language is one that doesn't have any
; side-effects. Since you have no side-effects there is no assignment
; operator and no terrible consequences of it. You can use a substitution
; like thing, programs are like mathematics and not like objects in the
; real world. There are a lot of wonderful things about functional
; languages: since there is no time you never have synchronization problems.
; And if you want to put something into a parallel algorithm you can run
; the pieces of that process in any way you want. There's never any 
; synchronization to worry about and it is very convenient environment for
; doing this.

; The price is you give up assignment.

; An advocate of a functional language would say: "Gee, that is just a tine
; price to pay. You probably shouldn't use assignment most of the time 
; anyway. If you just give up assignment you can be in this much much nicer
; world than this place with objects."

; Well, what is the rejoinder to that. Remember how we got into this mess.
; We got into this by hiding the state in the Cesaro compute pi program.
; We needed set! to encapsulate this logic in a module.

; A functional programmer would say you are thinking about modularity wrong.
; You are hung up on the idea of the next random number and the next random 
; number.

; Why wouldn't we just write an enumerator which would write an infinite
; stream of random numbers? And if you like you can put it through a
; processor, a Cesaro test, and what would come out is a stream of 
; successive approximations to pi.

RANDOM STREAM --> CESARO TEST --> STREAM OF APPROXIMATIONS TO PI

; We have them all at once and they get closer and closer to pi.

; Similarly, there are other things we tend to get locked in.
; This next one, next one and the next one. Which doesn't have to be that
; way. For example a banking system.

; A program that represents a bank account:
; The account might have in it in a sort of message passing way, we could
; say the program has some local state which is the balance. And a user
; using the system can send a transaction requests, and the account can 
; respond with what the balance is.

; Just like the random number generator you say we would like to have set!
; so we can have balance as a piece of local state inside the bank account,
; because we want to separate the state of the user from that of the
; bank account. Well, that's a message processing view.

; There's also a stream view of that thing without any set! or side effects.
; The idea is again, we don't think about anything having local state.

; We think about the bank account as something that is going to process
; a stream of transaction requests. Think about this bank account not as
; something that goes message by message, but something that takes in a 
; stream of for example successsive deposits.

4 2 2 1 --> BANK ACCOUNT --> 7 5 3 1

; So we think of the bank account not as something that has state, but
; as something that acts on an infinite stream of requests.

; And remember, we threw away time. So that when the user is here we can
; have these infinite streams of requests being generated one at a time
; coming from the user. And have the transactions coming back at the 
; printer coming back one at a time. And if we drew a line there the user
; couldn't tell the system does not have state.

; It looks just like the other one but there is no state in there.

; We call it make-deposit-account since you can only deposit. 

(define (make-deposit-account         ; It takes an initial balance and a 
         balance deposit-stream)      ; stream of deposits you might make.
  (cons-stream                        ; And it cons-streams
   balance                            ; the balance
   (make-deposit-account              ; onto make a new account stream.
    (+ balance (head deposit-stream)) ; Who's initial balance is the old
    (tail deposit-stream))))          ; balance plus the first thing in the
                                      ; deposit stream and who's deposit-
                                      ; stream is the rest.

; This is very much like the message-passing thing but this time without
; side-effects at all. There are very many things you can do this way.

; Can you do everything without assignment? Should we go over to only
; functional? Well, we don't know. But there seem to be places where purely
; functional programming breaks down. Where it starts hurting is where you
; have things like the bank account, but you also mix it up with the other
; things we have to worry about like objects and sharing - when two agents
; are the same.

; Suppose you want to extend the bank account:

; It takes a stream of transactions requests and puts out a stream of
; balances.

; Now suppose you want to model that it is a joint bank account between two
; independent people. Suppose there are two people Bill and Dave who have
; a joint bank account.

; Bill puts out a stream of transactions requests, and Dave puts out a
; stream of transactions requests. Somehow you have to merge them, so you
; might put out a stream processing thing called merge who sort of takes
; these two streams and puts them together, and now there are both talking
; to the same bank accoindependentunt. That's all great, but how do we 
; write merge?

; You want to do something that is reasonable.

; First guess might be to take alternating requests from Bill and Dave.
; But what happens when certainly in the middle of this Dave goes away
; on vacation for two years? Then Bill is stuck.

; What you want to do is what people call fair merge. It is sort of
; alternating but when nothing is coming you take one twice.

; You see I can not even say that without talking about time. So, one
; of the other active research areas in functional languages is inventing
; things like fair merge. Places which used to be taken up by side-effects
; and objects and sort of hide them away in very well-defined modules
; of the system. So all the problems of assignment are captured in some
; well-understood things.

; More generally we are running in a very basic problem in computer science.
; Which is: how define languages that somehow can talk about delayed
; evaluation, but also reflect on the fact that there are objects in the 
; world.

; That might be a very hard problem that has nothing to do with computer
; science. That it really is a problem of dealing with two very
; imcompatible ways of viewing the world.


; Questions:

; Are there techniques to localize effects of assignments?
; => I don't know. Well, certainly there is assignment inside memo-proc
;    but that was hidden away. And once the thing was triggered and gotten
;    an answer it will never change. So that was an  one time assignment.
;    This is something you can do.
;    One of the problem in for example fair merge is that you can begin
;    simulating assignment in the rest of the language. Any bit you built
;    in to built arbitrary stuff it is almost as bad as having assignment.
;    People are thinking about that now.

; I guess I don't see the problem with merge. Why do we need such a
; procedure? If we just add them it will come right?
; => We have a user who is interacting, but if the only way is to alternate
;    one might go away. And you're waiting at the bank account window 
;    waiting for the other guy since you cannot put in a request.

; Why is it waiting for one?
; => Because I have to define a function. What comes out of this merge box
;    is a function of what goes in.

; Why can't it be bound to time of the input?
; => Because I don't have time. See all I can say is that I am going to
;    define a function. There's no concept that it is going to alternate or
;    wait a while. You only have this stream of requests, the timeless
;    infinite stream of requests of all the requests that Dave would have
;    made. And the timeless infinite stream of requests that Bill would
;    have made. And I want to operate on them. That's how the bank acount
;    is working. The unfortunate thing is that those people that are sitting
;    at the bank account have the misfortune that they happen to exist in
;    time. They don't see their infinite streams of all the requests they
;    have ever made. They are waiting now and they want an answer.
;    That's the problem.

; Isn't time very important? For example the sequence of events: for
; example if Dave takes out 100 dollars it should reflect in the balance.
; => That's the thing I'm saying, this is an example where you can't. 
;    What comes out is a function of the stream going in here and the stream
;    going in here, and somehow some information about time, which is
;    precisely what a normal-order language won't allow you to say.

; Can't we just timestamp the inputs of Bill and Dave?
; => You can do that. Or you can for example do a read every microsecond.
;    But that is not solving the mismatch in the language with what we would
;    like to say.

;    Worse yet there are things that even fair merge cannot do. Managers
;    or something.
;    There is a whole research area that tries to push it as far till it
;    breaks down and you might have been using set! just as well.
