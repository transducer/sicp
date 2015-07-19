#lang scheme

;; We just looked at a strategy for implementing generic operators. That
;; strategy has a name, it is called DISPATCH ON TYPE.

;; The idea is that you break the system into a bunch of pieces (George
;; and Martha) and then there is the manager who looks at the type of the
;; data and then dispatches them to the right person.

;; Well, what criticisms can we make on this as a system organisation?
;; First of all there was the annoyance that George and Martha had to
;; change the name of their procedures. George originally had a real-part
;; procedure and he had to name it real-part-rectangular so it wouldn't
;; interfere with Martha's real-part procedure which is now named
;; real-part-polar who would not interfere with the manager's real-part
;; procedure who is now named real-part. That's kind of an annoying problem.
;; We will see that there are ways to package pieces so they will not
;; interfere with each other, we will not talk about that problem now.

;; The problem I want to focus on is the following: what happens if we bring
;; somebody new to the system? What has to happen? Well, George and Martha
;; don't care. George sits in the rectangular world, and Martha in the polar
;; world. But what would the manager have to do? If Harry comes in with a
;; new kind of complex number the manager has to change all its procedures.
;; The inflexibility of the system is in the manager. That's where work has
;; to happen to accomodate change.

;; That's even more annoying if you realize the manager is not doing
;; anything. "Is this type of number that George can handle, if so, send
;; it to George. Oh, is this something Martha can handle, if so, send it
;; of to Martha. It is all bureacracy, the manager is not doing any work.

;; Not an uncommon situation unfortunately. What really happening in the
;; system is that there is a table. There's types and there are operators.

;               POLAR         |       RECT
; --------------------------------------------
; real-part | real-part-polar | real-part-rect
; imag-part |
; mag       | mag-polar       | mag-rect
; angle     |

;; And so on.

;; All the manager is doing is acting as this table. Well, how do we fix
;; our system? We get rid of the manager, we are going to automate the
;; manager who basically consults this table, and have our system use the
;; table directly. Let's assume, using data abstraction, that we have some
;; type of data structure there is a table. And that we have an operation
;; called put and get.

;; Puts stores the value under key1 and key2:

(put key1 key2 value)

;; and get who can retrieve the value:

(get key1 key2)

;; And let's not worry about how tables are implemented. That's another
;; data abstraction. We will see later.

;; Given this organisation, what do George and Martha have to do?
;; They each have to set up their appropriate column in the table.

;; E.g., George:

;;; Installing the rectangular
;;; operations in the table

(put 'rectangular 'real-part real-part-rectangular)

(put 'rectangular 'imag-part imag-part-rectangular)

(put 'rectangular 'magnitude magnitude-rectangular)

(put 'rectangular 'angle angle-rectangular)

;; And Martha similarly:

;;; Installing the rectangular
;;; operations in the table

(put 'polar 'real-part real-part-polar)

(put 'polar 'imag-part imag-part-polar)

(put 'polar 'magnitude magnitude-polar)

(put 'polar 'angle angle-polar)

;; Everyone who has a representation needs to bring the column. Harry will
;; also bring a new column. And what happens to the manager? The manager
;; is automated out of existence and replaced by a procedure called operate,
;; this is the key operation in the whole system:

(define (operate op obj)
  (let ((proc (get (type obj) op)))  ; First tries to find a procedure in
    (if (not (null? proc))           ; the table using as keys the type and 
        (proc (contents obj))        ; the operator. If something is
        (error "undefined op"))))    ; restored then it will find the 
                                     ; procedure defined in the table and 
                                     ; apply it to the contents of the 
                                     ; object. If it cannot find it, return
                                     ; an appropriate error message.

;;; Defining the selectors using operate.

(define (real-part ob)
  (operate 'real-part ob))

(define (imag-part obj)
  (operate 'imag-part obj))

(define (magnitude obj)
  (operate 'magnitude obj))

(define (angle obj)
  (operate 'angle obj))

;; That's how the table plus the operate procedure replace the manager. 
;; Suppose I have one of Martha's complex numbers. We have z 1 or 2 and 
;; labeled polar. Suppose someone asks for the real-part of z.

(real-part z) ; Since this is defined in operate this is equivalent to:

(operate 'real-part z) ; Now operate comes and will look in the table.

((get 'polar 'real-part) ; And applies that to the contents of z.
 (contents z))

;; Operate does it uniformly, finds the right thing looks in the table and  
;; strips off the type and passes it down into the person who handles it.
;; This is a more flexible way of implementing generic operators and it is
;; called DATA-DIRECTED PROGRAMMING.

;; The idea is to in some sense have the data objects themselves carrying
;; with them the information on how you should operate on them.

;; Questions:
;; What do you have stored in that data object?
;; => There are more possibilities. In this particular implementation is the
;;    data itself and the symbol 'polar. Where the operations themselves are
;;    sitting in the table. The rows and colums are labeled by symbols. The
;;    key might be the symbol polar and the symbol magnitude. What I mean 
;;    here is the procedure magnitude-polar. What I could write is lambda.
;;    Martha and George do not need to name them. Or maybe I want to store all
;;    the operations in that data type: this is another way to organize data
;;    called MESSAGE PASSING.
