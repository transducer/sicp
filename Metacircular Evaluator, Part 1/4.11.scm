#lang planet neil/sicp

;; Instead of representing a frame as a pair of lists, we can represent a 
;; frame as a list of bindings, where each binding is a name-value pair. 
;; Rewrite the environment operations to use this alternative representation.

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; We really only need to change the constructors and access procedures for
; frames.

(define (make-frame variables values)
  (let aux ((vars variables)
            (vals values)
            (acc '()))
    (if (null? vars) acc
      (aux (cdr vars) (cdr vals)
           (cons (list (car vars) (car vals)) acc)))))

; testing

(define f (make-frame (list 'a 'b 'c) (list 1 2 3)))
f ; => ((c 3) (b 2) (a 1))

(define (frame-variables frame) (map car frame))

; testing

(frame-variables f)
; =>  (c b a)

(define (frame-values frame) (map cadr frame))

; testing

(frame-values f)
; =>  (3 2 1)

(define (add-binding-to-frame! var val frame)
  (let aux ((f frame))
    (if (null? f)
      (set-cdr! f (list var val))
      (aux (cdr f)))))

; Stuck here since the frame-values and frame-variables do not work by appending
; to the frame in this way.

; Looking at the solutions I realize that my assumption was wrong. The procedures
; set-variable-value!, lookup-variable-value and define-variable! also need to
; change after changing the implementation.

; testing

(add-binding-to-frame! 'd 4 f)
f

; See https://wqzhang.wordpress.com/2009/11/03/sicp-exercise-4-11/ for a solution.

