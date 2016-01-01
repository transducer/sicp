#lang scheme

;; Rewrite eval so that the dispatch is done in data-directed style. 
;; Compare this with the data-directed differentiation procedure of 
;; exercise 2.73. (You may use the car of a compound expression as the type 
;; of the expression, as is appropriate for the syntax implemented in this 
;; section.)

; Solution from http://community.schemewiki.org/?sicp-ex-4.3

(define operation-table make-table) 
(define get (operation-table 'lookup-proc)) 
(define put (operation-table 'insert-proc)) 
 
(put 'op 'quote text-of-quotation) 
(put 'op 'set! eval-assignment) 
(put 'op 'define eval-definition) 
(put 'op 'if eval-if) 
(put 'op 'lambda (lambda (x y) (make-procedure (lambda-parameters x) (lambda-body x) y))) 
(put 'op 'begin (lambda (x y) (eval-sequence (begin-sequence x) y))) 
(put 'op 'cond (lambda (x y) (eval (cond->if x) y))) 
 
(define (eval expr env) 
  (cond ((self-evaluating? expr) expr) 
        ((variable? expr) (lookup-variable-value expr env)) 
        ((get 'op (car expr)) (apply (get 'op (car expr) expr env))) 
        ((application? expr) 
         (apply (eval (operator expr) env) (list-of-values (operands expr) env))) 
        (else  (error "Unkown expression type -- EVAL" expr)))) 

