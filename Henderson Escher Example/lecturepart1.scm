#lang planet neil/sicp

;; The last time we talked about compound data.
;; There are two main points to that business:

;; 1. The methodology of data abstraction
;;    The point of which is that you can isolate the way that
;;    data objects are used from the way they are represented.
;; 2. There is this particular way that Lisp has of glueing
;;    together things with cons, car and cdr.
;;    And the way that cons, car and cdr are implemented is
;;    basically irrelevant.

;; As an example we looked at rational number arithmetic and 
;; vectors.

;; Using vectors:

(define (+vect v1 v2)
  (make-vector
   (+ (xcor v1) (xcor v2))
   (+ (ycor v1) (ycor v2))))

(define (scale s v)
  (make-vector (* s (xcor v))
               (* s (ycor v))))

;; Representation of vectors:

(define make-vector cons) ;; Define make-vector to be the thing 
(define xcor car)         ;; that cons is (get used to the idea
(define ycor cdr)         ;; that procedures are objects and you
;; can name them.

;; If this was all there was to it this would be pretty boring.
;; Remember that we can use cons to glue together arbitrary
;; things.

;; Let's define the arbitrary vector (2,3) to the vector
;; represented by the point (5,1).

;; Representing line segments:

(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

(make-segment 
 (make-vector 2 3)
 (make-vector 5 1))

;; If we peel away the abstraction layers we see a pair of pairs.

;; Just to remind you, there was this notion of closure.
;; Closure is the thing that allows us to build up complexity,
;; and that we did not get trapped in pairs.

;; The things that we make, having combined things with cons to
;; get a pair, those things themselves can be combined using
;; cons to get more complicated things. 
;; Or as a mathematician might say:

;; "The set of data objects in Lisp is closed under the operation
;; of forming pairs."

;; That is the thing that allows us to build complexity.

;; A lot of things that people make in computer programs are not
;; closed. BASIC and FORTRAN do not have array of arrays.

;; In any case, because we can form pair of pairs we can glue
;; things together in all sorts of ways.
;; It is a good idea to establish some kind of convention.

;; Lisp has a convention of making a sequence of things by
;; building a chain of pairs: a list.

;; LIST:

;; [ . | -]-> [ || -]-> [ || -]->[ || /]
;;   |          |         |        |
;;   v          v         v        v
;;   1          2         3        4

;; No more so I put a special marker that means there is nothing
;; more in the list. This is a conventional way to glue things
;; together if you want to form a sequence. The successive
;; cars are the items that you want to point together, and the
;; cdr pointer points to the next pair.

;; In Lisp you can construct that as:

(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))

;; This is a terrible drag to have to do that, so Lisp has an
;; operation that is called List, which is just an abbreviation
;; of this nest of conses:

(list 1 2 3 4)

;; That is a piece of syntactic sugar.

(define 1-to-4 (list 1 2 3 4))

;; Notice some of the consequences of using this conventions.
;; The car of this whole thing is the first element of the list.
;; How do I get two? Two would be the car of the cdr of 1-to-4
(car (cdr 1-to-4)) ;; => 2

;; Similarly:
(car (cdr (cdr 1-to-4)))       ;; => 3
1-to-4                         ;; => (1 2 3 4)
(cdr 1-to-4)                   ;; => (2 3 4)
(cdr (cdr 1-to-4))             ;; => (3 4)
(cdr (cdr (cdr (cdr 1-to-4)))) ;; => '()

;; This is called cdring down a list. We right procedures that
;; do that. 
;; One very common thing to do in Lisp is write a procedure that
;; goes down the whole list and does something to it and returns
;; a new list. E.g.,

;; (scale-list 10 1-to-4) ;; => (10, 20, 30, 40)

;; We need a recursive strategy for doing this.
;; Imagine you take the rest of the list (the cdr),
;; which would be scale-list cdr, then all I have to do is
;; multiply the car of the list by 10. And then cons that on
;; to the rest and I get a list. And go on.
;; Break when we get to the end of the list.

;; CDR-ing down a list and consing up the result:

(define (scale-list s l)
  (if (null? l) ;; <= is this thing the empty list pointer, are
      nil       ;;    there any elements in this list?
      (cons ( * (car l) s) ;; <= first element of scaled list
             (scale-list s (cdr l))))) ;; <= rest of scaled list

(scale-list 10 1-to-4) ;; => (10, 20, 30, 40)

;; This is a general pattern for doing something to a list.
;; You should know by now this means I should not be writing this
;; procedure itself. Here is the higher-order procedure that
;; does this called map.

(define (map p l)
  (if (null? l)
      nil
      (cons (p (car l)) ;; <= apply p to first element
            (map p (cdr l))))) ;; <= map down rest of the list

(define (scale-list-improved s l)
  (map (lambda (item) (* item s))
       l))
       
;; We are seeing patterns.

(map square 1-to-4) ;; => (1 4 9 16)
(map (lambda (x) (+ x 10)) 1-to-4) ;; => 11, 12, 13, 14

;; Doing something to each element in the list.
;; A thing you can think about is making map in an iterative
;; style. The one above involves a recursive process. We
;; can just as easily make one that uses a iterative process.

;; Stop thinking about the particular control structure in order.
;; This comes out of APL: stop thinking about control structures,
;; and start thinking about operations on aggregates. And about
;; halfway in this course you will see when this view of the
;; world really comes into its glory when we look at stream
;; processes. This is just a sort of cute idea, but we will see
;; much more applications later on.

;; There is something very similar to map. Map takes a list,
;; applies something to each item, and returns a list of the
;; successive values.

;; Another thing you can do which is very similar, is take a list
;; and some action you want to do, and then do it to each item
;; in the list in sequence. Don't make a list of the values, just
;; do this particular action.

;; That is something which is very much like map, but is called
;; for-each:

(define (for-each proc list)
  (cond ((null? list) *done*)
        (else (proc (car list)) ;; <= do it to first element
              (for-each proc ;; <= do it to the rest, and to the
                        (cdr list))))) ;; rest of the rest, etc.

;; Question: Does it makes copies?
;;           That is right. Map actually builds the collection of
;;           values.
;; Question: Could you build map from for-each
;;           I could arrange something.
;; Question: Difference of map and for-each irt recursion?
;;           For map you have to wait for the value. 
;;           For-each does not have to stick it to the beginning
;;           so that makes it an iterative proces here. You can
;;           make map iterative as well.