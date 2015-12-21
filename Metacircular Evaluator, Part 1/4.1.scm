#lang scheme

;; Notice that we cannot tell whether the metacircular evaluator evaluates 
;; operands from left to right or from right to left. Its evaluation order
;; is inherited from the underlying Lisp: If the arguments to cons in 
;; list-of-values are evaluated from left to right, then list-of-values
;; will evaluate operands from left to right; and if the arguments to cons 
;; are evaluated from right to left, then list-of-values will evaluate
;; operands from right to left.

;; Write a version of list-of-values that evaluates operands from left to 
;; right regardless of the order of evaluation in the underlying Lisp. 
;; Also write a version of list-of-values that evaluates operands from 
;; right to left. 

; Definitions from the book.

(define (rest-operands ops) (cdr ops))
(define (first-operand ops) (car ops))
(define (no-operands? ops) (null? ops))

; Below eval is replaced with display to not (yet) have to include all code

(define (eval exp env)
  (display exp))

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env))
            (rest (list-of-values-left-to-right (rest-operands exps) env))) 
        (cons first rest))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values-right-to-left (rest-operands exps) env))
            (first (eval (first-operand exps) env)))
        (cons first rest))))
