#lang scheme

; We so far looked at stream processing as generators. There is another
; way to look at stream processing:

; That's to not focus on process this elements as we walk down the stream, 
; but process the streams all at once.

; To show you what I mean I'll define two procedures that come in handy:

; add-streams takes two streams and will produce a stream who's elements 
; are the corresponding sums element wise.

(define (add-streams s1 s2)     
  (cond ((empty-stream? s1) s2)        ; If any of the streams is empty
        ((empty-stream? s2) s1)        ; we just return the other one.
        (else                          ; Otherwise, 
         (cons-stream                  ; we make a new stream, who's
          (+ (head s1) (head s2))      ; head is the sum of the two heads
          (add-streams (tail s1)       ; and who's tail is the result of
                       (tail s2))))))  ; recursively adding the tails.

; scale-stream takes a constant and a stream s and produces the stream of 
; of the elements of s multiplied by some constant.

(define (scale-stream c s)             ; scale stream just maps down the
  (map-stream (lambda (x) (* x c)) s)) ; stream with a function that 
                                       ; multiplies the element by a 
                                       ; constant.

; Given those two let me mean by programs that operate on streams all at
; once.

(define ones (cons-stream 1 ones))
; => Infinite stream of ones...

; The head is a one, and the tail is a list of who's first element is one,
; recursively.

(define integers                               ; The integers are a thing
  (cons-stream 1                               ; who's first element is 1
               (add-streams integers (ones)))) ; and the rest of them you
                                               ; get by taking those
                                               ; integers and incrementing
                                               ; them by one. So the second
                                               ; element of the integers
                                               ; is the first element of
                                               ; those integers incremented
                                               ; by one. And the third 
                                               ; element is the same as the
                                               ; head of the tail of them
                                               ; incremented by one, and
                                               ; incremented by one,
                                               ; and so on.

; Notice that this works because of delay. It looks like it could not have
; been processed. The reason that it works is because of that very
; sneaky hidden delay in there. What it really is:

(cons 1 (delay ones))

; While one is supposed to be defined as the first is one and the rest
; does not matter yet. Having run the definition ones is defined. Very
; sneaky. Integers the same way. We don't notice integers is not defined.

; It looks like a finite-state accumulator.

; Procedure to find the integral of a stream and produce a new stream.

(define (integral s initial-value dt) ; It takes an initial value and a time
  (define int                         ; constant. Internally we define int.
    (cons-stream                      ; So we can feed it back. It starts
     initial-value                    ; out with the initial value and the
     (add-streams (scale-stream dt s) ; rest of it is gotten by taken
                  int)))              ; the input stream, scale it by dt
  int)                                ; and adding it to int.

(define fibs                ; What are the Fibonacci numbers?
  (cons-stream 0            ; They're something that starts out with 0.
   (cons-stream 1           ; And the next one is 1 and the rest of 
     (add-streams fibs      ; them are gotten by adding the Fibonacci
          (tail fibs)))))   ; numbers to their own tail.

; We start of with someone who says: "Gee, compute for us the Fibonacci 
; numbers." We start with 0 and 1 and everything that is gotten after the
; 0 and 1 is gotten by summing two streams. One is the fibs themselves,
; and the other one is the tail of the fibs. So if I know that these start
; out with 0 and 1 I know that the fibs now start out with 0 and 1 and the
; tails of the fibs start out with 1.

;     fibs      0   1   1   2   3 /
;   tail fibs   1   1   2   3  / /
;                               /
; fibs | 0 1 |  1   2   3   5

; So it is a perfectly sensible definition.

; I can walk over to the computer and type print-stream fibs and they
; all coming flying out.

; It is a lot like learning recursion again. But instead of learning 
; recursive procedures we have recursively defined data objects. That
; shouldn't surprise you at all, since by now you should have come to
; believe that there is no difference, really, between procedures and data.
; And in fact, in some sense, the underlying streams are procedures
; sitting there. So because of the fact that we have recursive procedures 
; it should be natural that we have recursive data too.

; Unfortunately there are problems that streams are not going to solve.

; y1 = y2   y(0) = 1   dt = .001

; In the old days people built analog computers to solve these kind of 
; differential equations.

; We would like to write a stream program that can solve that...

(define y
  (integral
   dy 1 .001))

(define dy
  (map square y))

; There's a stream description. Unfortunately it does not work. Because
; when I define y to be the integral of dy it says, the integral of
; what? Oh that is undefined.

; I cannot write that definition before I got the other one, and vice
; versa.

; Is there a way out? We can do it with ones.

; The thing is cons-stream can do a useful thing there without looking at
; its tail. If we do cons-stream of 1 and something, without knowing
; anything about something we cannot built a self-referential definition
; with delay.

; Integral is a bit the same way...

(define (integral s initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream dt s)
                  int)))
  int)

; In the integral we know the initial value. So integral could be a
; procedure like cons-stream. You can define it and even when it knows
; what it is supposed to be integrating it knows what its initial value is.

; It does not have to look at the stream it is integrating until it is asked
; to work down the stream.

; We can write integrate with cons-stream:

(define (integral delayed-s             ; Exactly like previous, except
                  initial value         ; the stream it takes in is a 
                  dt)                   ; delayed object.
  (define int                           ; Inside is
    (cons-stream                        ; a cons-stream
     initial-value                      ; where only inside of it I start
     (let ((s (force delayed-s)))       ; looking at what the delayed value
       (add-streams (scale-stream dt s) ; is. So I add the initial value and
                    int))))             ; if anyone asks me for my tail, at 
  int)                                  ; that point I am going to force the
                                        ; delayed object, call that s, 
                                        ; and do the add-streams.

(define y
  (integral (delay dy) 1 .001))

; Now this will work, because it is the integral of something I don't
; care about right now because it is a delay. And once we define dy
; we can see the definition for y. Everything is started up with initial
; elements, and once I start mapping down the successive elements they
; are defined.

; This goes a bit beyond only using the delay that is hidden inside streams.
