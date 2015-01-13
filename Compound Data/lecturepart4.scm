#lang scheme

;; What we just have come off and done are some simple
;; examples of data abstraction. Now we are going to do something
;; more complicated. We are going to talk about what it means.
;; And this will be harder, because it is always much harder
;; in computer science to talk about what something means
;; than to go off and do it.

;; Let's go back to the beginning:
;; We just assumed there are procedures make-rat, numer and
;; denom and went off and defined rational number arithmetic
;; and we are done except of the implementation.
;; What is what is done? What was done was that we had defined
;; what are rational arithmetic in terms of 
;; abstract data. Any implementation of make-rat, numer and denom
;; could be the basis for rational number representation.

;; What suitable representation means is that:

;; if x = (make-rat n d)
;;   then 
;;    (numer x)   n
;;    --------- = -
;;    (denom x)   d

;; That is the contract. Your business is to find us three
;; procedures that fulfill this contract for any choice of n
;; d. That is what we mean by the rational number representation.

;; In fact, if we want to say what is a rational number really.
;; A rational number really is this axiom. 

;;       RATIONAL NUMBERS
;; ===========================
;; ===========================
;;            pairs

;; Let me do something which is going to terrify you. 1:02:22.

;; Let me bring you face to face with the existential reality
;; of this abstraction we are talking about.
;; What are pairs really? Only defined car cdr and cons.

;; Even though I lulled you into thinking that there is something
;; in Lisp which does that, in fact I did not tell you any more
;; about pairs than this tells you about rational numbers.
;; It is just some axiom for pairs.

;; => Go deeper.

;; We can build pairs out of nothing at all. Pure abstraction.

;; Implementation of cons, car and cdr:

(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (car x) (x 1))
(define (cdr x) (x 2))

;; There's no data in it. It is build out of air. What could that
;; possibly mean? If you really believe this stuff (pairs) then
;; we have to show it satisfies the axiom.

(car (cons 37 49)) ;; => 37

(car (lambda (pick)
       (cond ((= pick 1) 37)
             ((= pick 2) 49))))

((lambda (pick)
   (cond ((= pick 1) 37)
         ((= pick 2) 49)))
 1)

(cond ((= 1 1) 37)
      ((= 1 2) 49))

37

;; All data structures can be build on nothing. I could say 
;; in fact for various reasons there is a primitive if it would
;; make you feel better inside. But the point is it really
;; could work this way and not make any difference to the system
;; at all. We don't need data at all. We can do anything with
;; procedures.

;; Why did I terrify you
;; 1.) I want to show you this idea of abstraction.
;; 2.) We're going to see more and more of blurring the line
;;     between what is data and what is a procedure.

;; Lot of the very important programming techniques we are going
;; to show you depend very crucially on blurring the traditional
;; line between what you consider a procedure and what you
;; consider data.

;; Question: where is the source. You have to believe that
;; the procedures are objects. Suppose I asked for the
;; square root of 5. And then for the square root of 20.
;; You are not worried that you can apply square root to both.
;; You are thinking of a procedure that goes off and does
;; something. 
;; We are saying that procedures are not just the act of doing
;; something, but that procedures are conceptual entities:
;; objects. And if I build concs of 37 and 49 that is a
;; particular procedure that sits there and is different from
;; another one.

;; When I cons 37 and 49 and I do it again is that the 
;; same one and what is the difference and does it matter.

;; Same question as (= 2 2).

   
