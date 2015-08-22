#lang scheme

; We have a reasonable mechanical way how procedures can evolve a
; process. Now we are going to provide a way of pre-visualizing 
; how programs will look for processes.

;       Peano Arithmetic
; Two ways to add whole numbers:

; First two helper methods not defined in Racket:

(define (-1+ x)
  (- x 1))
(define (1+ x)
  (+ x 1))

(define (add x y)
  (if (= x 0)
      y
      (add (-1+ x) (1+ y))))

; "Moving marbles from one pile to the other till you run out."

; (+ 3 4)
; (+ 2 5)
; (+ 1 6)
; (+ 0 7)
; 7

(add 3 4) ; => 7

(define (add2 x y)
  (if (= x 0)
      y
      (1+ (add2 (-1+ x) y))))
; "Hold in your hand that what will be added later."

; (+ 3 4)
; (1+ (+ 2 4))
; (1+ (1+ (+ 1 4)))
; (1+ (1+ (1+ (+ 0 4))))
; (1+ (1+ (1+ (1+ 4)))
; (1+ (1+ 5)
; (1+ 6)
; 7

(add2 3 4) ; => 7

; add1 is straight
; add2 gets bigger and then smaller (things to be deferred)

; Amount of steps is time, width is space

; add1: time = O(x) space = O(1) (this is called lin. iteration)
; add2: time = O(x) space = O(x) (this is called lin. recursion)

; Two recursive definitions, but lead to different processes.

;       Bureaucracy
; Iteration:
; Lazy, he forwards the problem to himself. There is a new
; problem and return the result back to the original fellow.

; Recursion
; More bureaucrazy, keeps more people busy. He forwards the 
; problem to Harry who remembers that he will get something back.

; During iteration the process can be killed. It has state in
; explicit variables.
; Whereas recursion there is also information under the table.

;(define (CIRCLE X Y)
;  (PLOT X Y)
;  (CIRCLE (- X (* Y DT))
;          + Y ( * X DT)))
; In the end this will spiral out of control.

; Now we got some intuition on ways to solve things. Next we will
; use this intuition to build big hairy complicated systems.



