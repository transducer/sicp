#lang scheme

; Assignment
; State
; Change
; Time
; Identity
; Object
; Sharing

; Implications are absolutely frightening.

; (f x) might have a side-effect.

; We have to think about it mechanistically instead of mathematically.
; Philosophically harder.
; Programming is harder too because we have to think about sequencing and
; aliasing that don't exist in a language where we do not worry about
; objects.

; Modularity.

; We want to model reality.

; Maybe the technical problems have nothing to do with computers. Maybe
; we have the wrong view of reality. Maybe time is an illusion.

; Moving object has an unchanging existence in spacetime.

; Do not look instant by instant at the state, but look at the whole thing
; and the object that goes through it.

; Now we are going to look at a new way of viewing the world. More like the
; signal-processing view and less like objects that are sending messages.
; That's called stream processing.

; Make programs more uniform by throwing out unordinate concerns with 
; worrying about time.

(define (sum-odd-squares tree)
  (if (leaf-node? tree)
      (if (odd? tree)
          (square tree)
          0)
      (+ (sum-odd-squares
          (left-branch tree))
         (sum-odd-squares
          (right-branch tree)))))

(define (odd-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (odd? f)
              (cons f (next (1+ k)))
              (next (1+ k))))))
  (next 1))

; Two procedures with different structures, yet they are really doing 
; very much the same thing.

; The first procedure ENUMERATES LEAVES -> FILTERS ODDS -> MAP SQUARE
; (some kind of transducer) -> ACC + 0.

; The second procedure ENUMERATES INTERVAL -> MAP FIB -> FILTER ODD ->
; ACC CONS '().

; That commonality is completely obscured.

; The boxes do not appear in our programs.

; In order to control something you need the name of it.
; We don't have a good language to talk about them.
; Let's invent an appropriate language.

; What are the things that are flowing on the arrows?
; Those things are going to be data structures called STREAMS.

; What is a stream? A stream is a data abstraction so let's see its 
; selectors and constructors:

(cons-stream x y)
(head s)
(tail s)

; And the axioms that relate these:

; for any x + y
; (head (cons-stream x y))) -> x
; (tail (cons-stream x y))) -> y

; Exactly the axioms for cons, car and cdr.

; And we have

the-empty-stream  ; which is just like the empty list.
(empty-stream? s) ; just like the null test

; Why the new terminology? Why not call them lists? We will see that in a 
; while.

(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter pred
                          (tail s))))
    (else (filter pred (tail s)))))

(define (accumulate combiner init-val s)
  (if (empty-stream s)
      init-val
      (combiner (head s)
                (accumulate combiner
                            init-val
                            (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree
                   the empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream
       (head s1)
       (append-streams (tail s1)
                       s2))))

(define (enum-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (enum-interval (1+ low) high))))

; That's a little language for talking about streams. Let's express some
; things differently in it:

(define (sum-odd-squares tree)
  (accumulate
   +
   0
   (map
    square
    (filter odd
            (enumerate-tree tree)))))

(define (odd-fibs n)
  (accumulate
   cons
   '()
   (filter
    odd
    (map fib (enum-interval 1 n)))))

; We now have pieces we can start mixing.

; CONVENTIONAL INTERFACES
; that allow us to glue things together.

; Thesis research discovered:
; About 60% of programs can be expressed in MAP, FILTER and ACCUMULATE.

; Very much like APL, instead of arrays and vectors we have streams.
; Power of APL comes from these conventional interfaces.
