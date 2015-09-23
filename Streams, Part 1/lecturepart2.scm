#lang scheme

; Now two somewhat more complicated examples of streams.

; Suppose I got a stream and the elements of it are themselves streams.

; {{1 2 3...}{1 2 3...} ...{ }}

(define (flatten st-of-st)
  (accumulate append-streams
              the-empty-stream
              st-of-st))

(define (flatmap f s)  ; Each time I apply f to s I get a stream.
  (flatten (map f s))) ; If I map it all down I get a stream of streams
                       ; and I'll flatten that.

; given n : find all pairs 0 < j < i <= b
; such that i + j is prime

; e.g., N = 6   I  J  I + J
;               2  1  3
;               3  2  5
;               4  1  5

(flatmap
 (lambda (i)
   (map
    (lambda (j) (list i j))
    (enum-interval 1 (-1+ i))))
 (enum-interval 1 n))

; for each I we generate for each j for the interval 1 to i-1 we generate
; a pair and we map along the interval generating the pairs and we flatmap
; it. Then we got all the pairs and we test them.

(filter
 (lambda (p)
   (prime? (+ (car p) (cadr p))))
 (flatmap ...))

; We filter that thing we just built and see if we have an i and a j
; (first and second element) and then check if the sum is prime and filter
; the pairs. Then we go ahead:

(define (prime-sum-pairs n)
  (map
   (lambda (p)
     (list (car p)
           (cadr p)
           (+ (car p) (cadr p))))
   (filter ...)))

; We take the result and map along it generating the list i and j and 
; i + j. That is prime-sump-pairs.

; Nested loops start looking like compositions of flatmaps of flatmaps.

; This is awful so what you can do is introduce some syntactic sugar
; that is called collect.

(define (prime-sum-pairs n)
  (collect
   (list i j (+ i j))
   ((i (enum-interval 1 n))
    (j (enum-interval 1 (-1+ i))))
   (prime? (+ i j))))

; As i runs through the interval from 1 to n and j from 1 to i - 1 such
; that i + j equals prime. Collect is just that procedure of flatmaps of
; flatmaps.

; Famous problem:

; Backtracking problem of the eight queens problem. 
; Place eight queeuns so that none are attacking (not same diagonal or row).

; Represent board, represent positions, and

; assume there is a predicate called safe? which checks if it is safe to
; place a queen

(safe? <row> <column> <rest-of-positions>)

; Given a row and column and the rest of positions is it safe to put it
; there? Not hard to write, just check if previous are in the row, column or
; diagional.

; How can we organise it given that. 

; Backtracking. Start with the first column and first row.
; Given it is in the first column, check the next queen if not possible
; back up, put the first queen in the next, et cetera. As far down the tree
; as I can and backup and generate the subtree. If I manage to go all the
; way down I have found the solution.

; BACKTRACKING SEARCH

; This is unnecessary... Why is it complicated? Somehow the program is too
; unordinately concerned with time. If I stop worrying about time so much
; then there is a much simpler way to describe this.

; Let's imagine I have in my hands the tree of all possible ways to put down
; queens in the first k columns. Then how do I find all ways to find queens
; in the next position? I filter everytime for all the fields that are safe.
; Recursive strategy for solving. Let's look at it.

(define (queens size)   ; to solve the 8-queens problem on a board of size
  (define (fill-cols k) ; size, we have a sub procedure that fills columns
    (if                 ; and shows how to put down queens safely.
     (= k 0)            ; when k is equal to 0 I don't put anything down.
     (singleton empty-board)
     (collect ...)))    ; Otherwise I collect...
  (fill-cols size))

(collect                    ; I collect together
 (adjoin-position try-row   ; the new row I am going to try and make a new
                  k         ; position collection
                  rest-queens)
 ((rest-queens (fill-cols (-1+ k))) ; find all ways to put down queens
  (try-row (enum-interval 1 size))) ; in the first k minus 1 columns
 (safe? try-row k rest-queens))     ; find all ways to trying the row.

; This gets all solutions to the eight-queens problem. 

; We threw away all the stuff around it in time and space. We changed our
; view of what it is we are trying to model. We are no longer thinking about
; something that has steps or state, instead we try to model the global
; thing. The whole flight of the chalk.

; Question: Isn't it faster to do it the first way?

; You should start getting suspicious at this point. Isn't it wonderful but
; isn't it terrible inefficient?
