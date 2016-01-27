#lang scheme

;; Let expressions are derived expressions, because

; (let ((<var1> <exp1>) ... (<varn> <expn>))
;   <body>)

;; is equivalent to

; ((lambda (<var1> ... <varn>)
;    <body>)
;  <exp1>
;  
;  <expn>)

;; implement a syntactic transformation let->combination that reduces evaluating let 
;; expressions to evaluating combinations of the type shown above, and add the 
;; appropriate clause to eval to handle let expressions.

(define (let->combination let-exp)
  (let* ((lets (cadr let-exp))
        (body (cddr let-exp))
        (vars (map car lets))
        (exps (map cadr lets)))
  (make-lambda vars body exps)))
    
; Testing

(let->combination 
  '(let ((foo 3) 
         (bar 1)) 
     (+ foo bar)))
; => ((lambda (foo bar) (+ foo bar) 3 1))

; Include it in eval

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((let? exp) (eval (let->combination exp) env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))

