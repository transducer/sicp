#lang scheme

;; Let* is similar to let, except that the bindings of the let variables are 
;; performed sequentially from left to right, and each binding is made in an 
;; environment in which all of the preceding bindings are visible. For example

; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))

;; returns 39. Explain how a let* expression can be rewritten as a set of 
;; nested let expressions, and write a procedure let*->nested-lets that 
;; performs this transformation. If we have already implemented let 
;; (exercise 4.6) and we want to extend the evaluator to handle let*, 
;; is it sufficient to add a clause to eval whose action is

; (eval (let*->nested-lets exp) env)

;; or must we explicitly expand let* in terms of non-derived expressions?

; => We can create a transformation by nesting a let expression by consing
; => up to another let expression. I.e., the above let* transforms into
; (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5) (* x z))))))

; Showing above

(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))
; => 39

; Implementation:

(define (let*->nested-lets expr)
  (let* ((lets (reverse (cadr expr))) ; Reverse so last let will be inner
        (body (caddr expr))
        (vars (map car lets))
        (vals (map cadr lets)))
    (let loop ((vars vars)
               (vals vals)
               (acc body))
      (if (null? vars) acc
        (loop (cdr vars)
              (cdr vals)
              (make-let (car vars) (car vals) acc))))))

(define (reverse l)
  (foldl cons '() l))

(define (make-let var val body)
  (list 'let (list (list var val)) body))


; Testing

(let*->nested-lets '(let* ((x 3) 
                           (y (+ x 2)) 
                           (z (+ x y 5))) 
                      (* x z)))
; => (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))

