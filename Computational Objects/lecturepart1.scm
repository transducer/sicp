#lang scheme

; Now that we have given you some power to make independent local state
; and to model objects. Thought we do a bit of programming of a very 
; complicated kind to illustrate what you can do with this sort of thing.

; I suppose as I said we are motivated by physical systems. We like to
; think of them as things with independent local state. That makes it a 
; thing. We are going to say that we have a world, and a model of that
; world. I want to make a correspondence between them.
; The functions that relate things in the world with the functions in the
; computer. This buys us modularity.

; We can arrange our world to be like that. Then we can inherit the 
; modularity in the world in our programming. That is why we invented
; object-oriented programming.

; Let's look at the most wonderful objects: electrical systems. These are 
; the physicist best objects. Over here I have a piece of machinery.
; It's got electrical wire. One of the most wonderful properties of the
; electrical world there are no connections between things accept for the
; wires. The connection is clear. There are no other connections that
; I know of. Making a knot in the wire does not matter either. The way that
; the physics is arranged is that the connection can be made abstract.
; We have captured all of the connections there are.

; Let's talk about the most abstract type of physical objects: electrical
; circuits:

;     Primitives and Means of Combination

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)  ; Or gate with input wire a and b and output wire d.
(and-gate a b c) ; And gate with input wire a and b and output wire c.
(inverter c e)
(and-gate d e s)

; We connect them together with wires. We do not care about their physical
; properties (voltage, water). We build systems by wiring them together.

; Now we are going to build an embedded language in Lisp. Same as the
; Henderson picture language. Different from the pattern matching and
; substitution. The latter was interpreted by a Lisp program. The 
; embedding of Henderson was build up in Lisp.

; We have some primitive type of objects and we use wire to combine them.
; We can make wires with make-wire. We build digital circuits with these 
; primitives. 

;     Means of Abstraction

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

; Half-adder is used for adding numbers together of A and B and putting out
; a sum and a carry.

; The half adder adds two single binary digits A and B. It has two outputs, 
; sum (S) and carry (C). The carry signal represents an overflow into the 
; next digit of a multi-digit addition. (Wikipedia)

; In fact the wiring diagram is as we told you. The boundary is the box.
; An abstraction is always a box. The things that come out of it are the
; declared variables of a lambda expression, the lambda expression that 
; defines half-adder. Internal to that I make up some more wires. D and E
; which I am going to use for the interconnect. The interconnect that does
; not come through the walls of the box. And we wire things together as
; we just saws.

; The nice thing about is is that this language is hierarchical in the right
; way. If a language is not hierarchical in the right way - if it turns out
; that a compound objects does not look like a primitive - there is
; something wrong with the language. (At least that is the way I feel about
; that.)

; Instead of having a mathematical functions or things that compute
; mathematical functions (what we had up to now), we are now starting
; things that are electrical objects and we are building up more electrical
; objects. And the glue we are using is basically the Lisp structure:
; lambda's. Lambda is the ultimate glue.

; And of course half-adder itself can be used in a more complicated 
; abstraction like a full-adder. Two half-adder hooked together with an
; or-gate that take some input numbers, a carry-in and produces output
; a sum and a carry-out.

; A full adder adds binary numbers and accounts for values carried in as 
; well as out. A one-bit full adder adds three one-bit numbers, often 
; written as A, B, and Cin; A and B are the operands, and Cin is a bit 
; carried in from the previous less significant stage. (Wikipedia)

;     Means of Abstraction

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

; Out of full-adders you can make adders chains and big adders...

; So we have a language that has primitives, means of combination and
; means of abstraction. It is a real language. So how are we going to
; implement this?

; Let's do it easily. The only problem is that we have to implement the
; primitives. Nothing else has to be implemented. :)

; Because we are picking up the means of combination and means of 
; abstraction from Lisp. Inheririting them from the embedding.

; Let's look at the Inverter (a particular primitive).

;     Implementing a Primitive

(define (inverter in out) ; Two wires coming in, an in and an out
  (define (invert-in)     ; they know what to do when wires comes in.
    (let ((new            
           (logical not (get-signal in))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! in invert-in))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "invalid signal" s))))

; The input wire is told "When you change, tell me". The object that is
; the inverter has to tell the object that is the input wire "Hi, my name
; is George, and my job is to do something with the results when you change.
; So when you change, tell me about it, cause I got to do something with 
; that". That is done by add-action! by adding an action to the input 
; wire called invert-in where invert-in is a procedure of no arguments
; that gets the logical not on the signal of the input wire and after 
; some delay with is the inverter delay we'll set the signal on the output
; wire to the new value.

; A very simple program. The output wire has to be sensitive though, so
; it might be that the output wire has to communicate further.

; And we can look at things more complicated like and gates. And gates take
; two inputs and produce an output. Identical to what we just saw.

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output
                                  new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

; set-signal! computes the logical and of the signals of the inputs, and 
; after some delay (the and-gate-delay) calls the set-signal! which sets
; the signal on the output to a new value. How I implement these things is
; all wishful thinking.

; I have an assignment operator called set-signal!, a derived assignment
; operator. By convention labeled with an exclamation mark.

; Below I have add-action! which is used to inform the wire called a1
; to call the and-action-procedure when things changed (same for a2).

; Let's talk about this communication that must occur between these various
; parts. Suppose I have a simple circuit which contains an AND with wires
; a and b which connects to a wire called c to an inverter which has
; a wire output called d. 

; a,b AND c INVERTER d

; This is an abstraction of the phyical world. I can get these pieces at
; Radioshack for a few cents.

; Now, suppose I try to find the computational model which corresponds to
; the above part of reality. Find relationships and abstractions in the
; computer. That is the goal.

; I have some sort of thing called the signal a. And a signal b. Now these
; signals can be hooked together in an AND gate. The 
; and-gate-action-procedure. Which is going to interact with a wire object
; which we call c. Which is going to connect to another action procedure
; called the inverter. We end up with another wire we call d. 

; What's inside this layer of stuff? There will be some variable inside 
; the signals that holds some value. So there must be some kind of 
; environment associated with it. For each one of each there must be
; an environment that binds signal (a,b,c). Presumably signal is a variable
; that is either 0 or 1.

; We also have to have some list of people to inform if the signal changes.
; We have to inform AND for example. The action procedure (AP). It is
; presumably a list, and the first thing on the list is to inform this (AND)
; guy.

; There might be other people who are looking at it. The other guys attached
; to it. And of course c and d also have some APs.

; It is also the case that when the AND procedures is awakened - one of the 
; people you have told to wake you up when there signals changes wakes you
; up - you have to know what is their signal. So you can do the AND and
; produce a signal for c. So there has to be information in the AND like
; my a1 is this guy, and my a2 is this guy. And when I choose my AND
; I have to tell this guy something. So I need an output.

; Similarly for Inverter. It needs an input that interrogates what the value
; is, and when he gets that value he has to say "okay, output changes this
; guy", and so on. We need at least that much connectedness.

; Let's look at the AND gate above.

; At the time and-gate is called a frame of a1 and a2 and output create a 
; frame which binds the values to the wire values that were passed in.
; In that environment we construct the procedure and-action-procedure.
; The result of evaluating a lambda expression. It hangs on to the frame
; in which the variables were defined. Its local state is that. Therefore
; the and-action-procedure has access to them.

; Let's look at a wire. A compicated mess:

(define (make-wire)
  
  ; A wire needs two things that are part of it (its state): the signal
  ; and the action procedures of the wire.
  (let ((signal 0) 
        (action-procs '()))
    
    ; In that context we define various procedures
    ; One is called set-my-signal! to set a signal to a new value.
    (define (set-my-signal! new)  ; It takes a new value
      (cond ((= signal new)       ; If that is equal to the current value of
             'done)               ; my signal I am done.
            (else                 ; Otherwise,
             (set! signal new)    ; I set the signal to the new value
             (call-each action-procs)))) ; and call each of the action 
                                         ; procedures that I have been
                                         ; introduced when the AND gate
                                         ; was applied to me by add-action
                                         ; procedure at the bottom.
    
    ; Also, I need a way of accepting an action procedures.
    (define (accept-action-proc proc) ; Which increments the action
      (set! action-procs              ; procedures using set
            (cons proc action-procs)) ; to the result of consing up a new
      (proc))                         ; procedure which is passed to me to
                                      ; my action procedures list.
                                      ; And for technical reasons I have to
                                      ; call that procedure once (to get it 
                                      ; started).
    
    ; And finally I need this thing called the dispatcher which is a way
    ; of attaching a message to a wire which is going to be used to extract
    ; various information from it, like:
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal) ; What is the current signal value?
            ((eq? m 'set-signal!)        ; What is the method of setting
             set-my-signal!)             ; your signal?
            ((eq? m 'add-action!)        ; How do I add another action
             accept-action-proc)         ; procedure?
            (else
             (error "Bad message" m))))
    dispatch))

; So the wire I have constructed is a message accepting object which 
; accepts messages like "What is your method of adding action procedures?", 
; which will give a procedure called the add-action-procedure which I can 
; then apply to an action procedure to create another action procedure in 
; the wire (a permission).

; We can see that over here:

(define (call-each procedures)     ; cdring down the list
  (cond ((null? procedures) 'done)
        (else
         ((car procedures))
         (call-each (cdr procedures)))))

(define (get-signal wire) ; For getting a signal I call the dispatch of the
  (wire 'get-signal))     ; wire with the message 'get-signal.

(define (set-signal! wire new-value) ; If I want to set it (change it) then
  ((wire 'get-signal!) new-value))   ; I ask for permission to set the
                                     ; signal and use that permission 
                                     ; (which is a procedure) on the new 
                                     ; value. The set-my-signal which
                                     ; is returned uses the internal 
                                     ; variable of the wire. Then all the 
                                     ; action procedures are called waking
                                     ; them up.

(define (add-action! wire action-proc) ; We also have a way for adding
  ((wire 'add-action!) action-proc))   ; actions. We accept a wire and an
                                       ; action procedure. And we ask the
                                       ; wire for permission to add an
                                       ; action. Getting that permission we
                                       ; use it to give it an action 
                                       ; procedure.

; This is a real object. There is a few more details, like for example
; how am I going to control this thing. How am I going to do these delays?
; Let's look at that:

; We know there is a delay in the AND gate that when a signal changes
; on the input there is a delay, and that it is then going to call a
; procedure which is going to change the output.

; This is a fairly complicated mechanism.

(define (after-delay delay action)     ; A delay is a number and an action
                                       ; is a procedure.
  ; They have a special structure called an agenda which is a thing that
  ; organises time and actions. We'll see that later.
  ; The agenda has a moment at which something happens.
  (add-to-agenda!                      ; Here we are setting up for later
   (* delay (current-time the-agenda)) ; at some moment to do
   action                              ; this action
   the-agenda))                        ; and add it to the agenda.

; The way this machine will run is simple: we have a thing called propagate
; which defines how things are run.
(define (propagate)
  (cond ((empty-agenda? the-agenda)       ; If the agenda is empty we are
         'done)                           ; done.
        (else                             ; Otherwise, 
         ((first-item the-agenda))        ; we take the first item of the
                                          ; agenda (a procedure with no 
                                          ; arguments). We call that with
                                          ; no arguments (the extra 
                                          ; parentheses)
         (remove-first-item! the-agenda)  ; Then we remove that first item
         (propagate))))                   ; from the agenda and we go around
                                          ; the propagation loop. 

; This is the overall structure of this thing. There are a few other things
; we can look it, then we are going to look into the agenda.

; By the way, you may think that the simulator is very simple, and probably
; to simple to be useful. The fact of the matter is that this simulator
; has been used to manufacture a fairly large computer. So this is a real
; life example (although there were many more primitives, e.g., flip 
; flops, matches, transparent latches). The difficulty with that is that
; there were pages and pages of definitions of all these primitives
; and there were many more parameters like setup times and hold times.
; But with the exception of that part of the complexity the structure of
; the simulator we used for building a real computer that works is exactly
; the same as what we are seeing here.

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
; => SUM 0  NEW-VALUE = 0

(probe 'carry carry)
; => CARRY 0  NEW-VALUE = 0

; A special kind of objects called a probe is thrown onto some wires:
; a probe is an object that has the property that when you change a wire
; it is attached to it types out a message.

; Once you call the probe one it says the current value of SUM at time 0
; is 0. And the value of the carry at time 0 is 0.

; Then we go off and build some structure:

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)

(propagate)                  ; Some propagation at time 8 make it become 1.
; => SUM 8  NEW-VALUE = 1
;    DONE

(set-signal! input-2 1)

(propagate)                  ; At time 11 the carry was one and the sum at
; => CARRY 11  NEW-VALUE = 1 ; 16 was 0. This is true of the digital 
;    SUM 16  NEW-VALUE = 0   ; circuitry. It works. That is the kind of 
;    DONE                    ; behaviour we get out of this thing.

; So what I've shown you is a large scale picture how you implement an
; event-driven simulation of some sort. How you might organise it to have
; nice hierarchical structure which allows you to build abstract boxes
; that you can instantiate. I haven't told you any of the details of how
; the agenda and things like that work. That is what we'll do next.
; That is going to involve change and mutation of data and things like that.

; Questions? [Wait...] Thank you. Let's take a break.



  