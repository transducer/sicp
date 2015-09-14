#lang planet neil/sicp

; Redo exercise 3.18 using an algorithm that takes only a constant amount 
; of space. (This requires a very clever idea.) 

; Detect cycle in singly-linked list: 
; https://www.youtube.com/watch?v=kSryp033xLA

; Use tortoise and hare algorithm (description based on Smitha Milli's 
; YouTube video): 

; - Two pointers (tortoise and hare) start from the beginning of the list.
; - Tortoise moves one node at a time, hare two.
; - If the hare and rabbit ever meet you know there has to be a cycle. 
;   Because the hare is going twice as fast it will loop around the cyclic 
;   part of the list twice as fast as the tortoise. Which means that 
;   eventually it has to catch up.
; - If the tortoise gets to the end you know that the list does not have a 
;   cycle in it, because it reached the end.

; Solution taken from http://mitchellkember.com/blog/post/tortoise-and-hare/

(define (detect-cycle x0)
  (define (race t h)
    (cond ((or (null? t) (null? h) (null? (cdr h))) #f)
          ((eq? t h)
           (list (find-mu t x0)
                 (find-lambda t (cdr h))))
          (else (race (cdr t) (cddr h)))))
  (define (find-mu t h)
    (if (eq? t h)
        0
        (+ 1 (find-mu (cdr t) (cdr h)))))
  (define (find-lambda t h)
    (if (eq? t h)
        1
        (+ 1 (find-lambda t (cdr h)))))
  (if (or (null? x0) (null? (cdr x0)) (null?  (cddr x0)))
      #f
      (race (cdr x0) (cddr x0))))


; Testing:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair lst)
  (if (null? (cdr lst)) 
      lst
      (last-pair (cdr lst)))) 

(define lst (list 'a 'b 'c))
(detect-cycle lst)
; => #f

(define cycle (make-cycle lst))
(detect-cycle cycle)
; => (0 3)
