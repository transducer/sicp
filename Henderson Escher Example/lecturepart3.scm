#lang planet neil/sicp

;; Well, why do I say this example is nice. You probably think
;; it is more weird than nice. Representing these pictures as
;; procedures which do complicated things with rectangles.
;; So why is it nice?

;; The reason it is nice is that once you have implemented the
;; primivites in this way, the means of combination just fall out
;; by implementing procedures.

;; Suppose I want to implement beside.

;; I've got a picture p1 (if you hand a picture a rectangle it
;; causes an image to be drawn into the rectangle you give it.

;; p1 : [ ] -> [ A ]
;; p2 : [ ] -> [ B ]

(beside p1 p2 a) ;; Picture 1 and 2 and scale factor a.

;; Take rectangle, and split it into two. Then it says, now I
;; got two rectangles and goes off to P1 and says draw yourself
;; in the left rectangle, and p2 draw yourself in the right one.
;; The only computation is has to do is know what the rectangles 
;; are (remember, these are specified by origin, horizontal
;; vector and vertical vector).

;; [a| B ]

;; Let's look athe piece of code:

(define (beside p1 p2 a)
  (lambda (rect) ;; Since it is a picture it takes a rect as arg.
    (p1 (make-rect ;; <= p1 draw yourself in some rectangle.
         (origin rect)
         (scale a (horiz rect))
         (vert rect)))
    (p2 (make-rect  ;; <= and p2 too do it in another rectangle.
         (* rect (origin rect)
            (scale a (horiz rect)))
         (scale (- 1 a) (horiz rect))
         (vert rect)))))

;; Similarly rotate:

(define (rotate90 pict)
  (lambda (rect) ;; <= the procedure which again says draw
    (pict (make-rect ;; yourself in some rectangle.
           (* rect (origin rect) ;; <= the algebra to rotate the
              (horiz rect))      ;;    vector. Origin some place
           (vert rect)           ;;    else, and horizontal and
           (scale -1 (horiz rect)))))) ;; vertical 
;; vector some place else.

;; Notice, the crucial thing that is going on here is that you
;; are using the representation of pictures as procedures to
;; automatically get the closure property.
;; Beside has this thing p1, p1 does not care if p1 is a 
;; primitive picture, a line segment, or itself a result of
;; some rotation. All beside has to know about p1 is that when
;; you give p1 a rectangle it causes something to be drawn.
;; And above that level it is none of its business how p1 
;; accomplishes its drawing.
;; You are using the procedural representation to ensure this
;; closure.

;; Pictures as procedures makes this means of combination both
;; pretty simple and I think elegant.

;; This is not the real punchline. The real punchline comes when
;; you look at the means of abstraction in this language.
;; Because, what have we done?
;; We implemented the means of combination themselves as 
;; procedures. Which this means is that we go to abstract,
;; everything that Lisp supplies us for manipulating 
;; procedures is automatically available to do things in this
;; picture language.
;; Not only is this language implemented in Lisp. It is nicely
;; embedded in Lisp. What I mean is that when embedding the
;; language in this way, all the power of Lisp is automatically
;; available as an extension to what you want to do.
;; What do I mean? Example:

;; Suppose I want to make a thing that takes four pictures.
;; And makes a configugration in a square (four-picts) and also
;; takes parameters for rotating. You can do this easily.
;; It comes from the embedding.

;; Suppose I want to use recursion. Let's use at a recursive
;; means of combination on pictures.

(define (right-push p n a) ;; picture p, integer n, 
  (if (= n 0)              ;; scale-factor a
      p
      (beside p (right-push p
                            (-n 1)
                            a)
              a)))

;; How did I get this fancy recursion? Absolutely automatic.
;; The embedding says yes I can arrange recursive procedures.

;; We are embedding something in a language.
;; Lisp is a lousy language for doing any particular problem but
;; it is good for figuring out the right language that you want
;; and embedding that in Lisp.
;; That is the real power to this approach of this design.

;; The other thing we can do in Lisp is capturing general methods
;; of doing things as higher-order procedures.

;; Just to illustrate and give you a convulated use of higher-
;; order procedures, let's give you the general idea of using
;; recursion in a higher-order procedures.

(define (push comb) ;; <= where comb is means of combination
  (lambda (pict n a)
    ((repeated 
      (lambda (p) (comb pict p a))
      n)
     pict)))

;; The result of this procedure repeated is what I apply to
;; picture.

(define right-push (push beside))
(define up-push (push above)) ;; et cetera, you can do anything.

;; There is a lot to learn from this example. 
;; 1. Embedding. Lisp as natural extension of language that you
;;    build in Lisp.
;; 2. Procedure takes procedure, that pict.. Et cetera,
;;    there is no difference between a procedure and data,
;;    it is really both or neither in some sense.
;; 3. Viewing the engineering design proces as creating a
;;    language. A methodology, a mythology, which is called
;;    software/engineering. Figure out exactly what you do, 
;;    this is three sub tasks, that exists of three, go back
;;    specify that out. And you end up with a marvelous tree
;;    where you broken your task into subtasks and so on and
;;    each of this node which is defined exactly as specified.
;;    Only a computer scientist can think this is actually how
;;    it works. In reality there is a *layer of languages*.
;;    A language of primitive pictures (line-segments and 
;;    particular pictures in the unit square). On top of that was
;;    a whole language of geometric combinators, of geometric
;;    positions which talk about above, beside, right-push and
;;    rotate. They talk with reference about the things that are
;;    talked about in the primitive language. Above that there
;;    is a scheme of combinations. For example push, which talked
;;    about repeatedly doing something with a scale factor.
;;    At each level the things that are talked about are the
;;    things that were erected at the previous level.
;;    Difference with the computer science subtask engineering
;;    mythology, is that you know have a full range language.
;;    It is not set up to do a particular task, but it has
;;    a whole language to do particular things.
;;    When you change something it is more probable that you can
;;    express it in your vocabulary.
;;    Levels of languages instead of strict hierarchy.

;; The design process is not so much implementing programs as
;; it is implementing languages.
