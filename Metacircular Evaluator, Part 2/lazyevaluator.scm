#lang planet neil/sicp

; Source http://www.giovannisfois.net/content/sicp-exercise-433

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((unbound? exp)    (eval-unbound exp env))
        ((if? exp) (eval-if exp env))

        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))

        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp)  (eval (cond->if exp) env))

        ((while? exp)  (eval (while->let exp) env))

        ((let? exp)  (eval (let->lambda exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))

        ((application? exp)
         (metacircular-apply (actual-value (operator exp) env)
                             (operands exp)
                             env))

        (else
          (error "M-Eval: Unknown expression type -- EVAL: " exp))))

(define (metacircular-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (metacircular-apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env) ; changed
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first (eval (first-operand exps) env)))
      (cons first
            (list-of-values (rest-operands exps) env)))))

;-----------------------------------------------
;LAZY
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
          (list-of-arg-values (rest-operands exps)
                              env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env)
          (list-of-delayed-args (rest-operands exps)
                                env))))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;------------------------------------

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;-----------------------------------------------

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-unbound exp env)
  (make-unbound (definition-variable exp) env)
  'ok)


;-------------------------------
; Syntax

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;---

;Weiqun Zhang 4.33 
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
      (eval (list 'cons 
                  (list 'quote (car text))
                  (list 'quote (cdr text)))
            env)
      text)))

;----
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;----

(define (definition? exp)
  (tagged-list? exp 'define))

(define (unbound? exp)
  (tagged-list? exp 'unbound))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) ; formal parameters
                 (cddr exp)))) ; body

;------
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;-------

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;-------------------
(define (and? exp) (tagged-list? exp 'and))

(define (eval-and-operands exp env)
  (if (no-operands? exp)
    true
    (if (false? (eval (first-operand exp) env))
      false
      (eval-and-operands (rest-operands exp) env))))

(define (eval-and exp env)
  (eval-and-operands (operands exp) env))

;------------------

(define (or? exp) (tagged-list? exp 'or))

(define (eval-or exp env)
  (eval-or-operands (operands exp) env))

(define (eval-or-operands exp env)
  (if (no-operands? exp)
    false
    (if (true? (eval (first-operand exp) env))
      true
      (eval-or-operands (rest-operands exp) env))))

;-------------------

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;----------

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;----------

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;-----------
; cond -> if
;

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false ; no else clause
    (let ((first (car clauses))
          (first-actions (cond-actions (car clauses))) 
          (rest (cdr clauses)))

      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "M-Eval: ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (if (eq? '=> (car first-actions))
                   (list (cadr first-actions) (cond-predicate first))
                   (sequence->exp (cond-actions first)))
                 (expand-clauses rest))))))

;---------------
(define (while? exp)
  (tagged-list? exp 'while))

(define (while-test exp)
  (cadr exp))

(define (while-body exp)
  (cddr exp))

(define (while->let exp)
  (list 'let 'loop '()
        (list 'and 
              (while-test exp)
              (sequence->exp (while-body exp))
              (list 'loop))))

;---------------
(define (let? exp)
  (tagged-list? exp 'let))

(define (named-let? exp)
  (symbol? (cadr exp)))

(define (let-name exp)
  (cadr exp))

(define (let-clauses exp)
  (if (named-let? exp)
    (caddr exp)
    (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
    (cdddr exp)
    (cddr exp)))

(define (let-vars clauses)
  (map car clauses))

(define (let-values clauses)
  (map cadr clauses))

(define (let->lambda exp)
  (if (named-let? exp)
    (list 'begin 
          (list 'define 
                (cons (let-name exp) (let-vars (let-clauses exp)))
                (let-body exp))
          (cons (let-name exp) (let-values(let-clauses exp))))

    (cons (cons 'lambda 
                (cons (let-vars (let-clauses exp))
                      (let-body exp)))
          (let-values(let-clauses exp)))))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (clauses->nested-lets clauses body)
  (if (null? clauses)
    body
    (list 'let 
          (list (car clauses)) 
          (clauses->nested-lets (cdr clauses) body))))

(define (let*->nested-lets exp)
  (clauses->nested-lets (let-clauses exp) (let-body exp)))


;---------------

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;----------------

(define (make-procedure parameters body env)
  (list 'procedure parameters  (scan-out-defines body) env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;---

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'display display)
        (list 'newline newline)
        (list 'assoc assoc)
        (list 'list list)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list '= =)
        (list '> >)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (metacircular-apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

;--------------

;--------------
;ENV

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (map cons variables values))

(define (add-binding-to-frame! var val frame)
  (if (null? (cdr frame))
    (set-cdr! frame (list (cons var val)))
    (add-binding-to-frame! var val (cdr frame))))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "M-Eval: Too many arguments supplied" vars vals)
      (error "M-Eval: Too few arguments supplied" vars vals))))

(define (look-for-var var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (car frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      '()
      (let ((frame (first-frame env)))
        (scan frame))))
  (env-loop env))

(define (scan-out-defines exp-body)
  (let ((var-list '())
        (new-body '()))
    (define (scan-body token)
      (cond ((definition? token) 
             (set! var-list (cons (list (definition-variable token)  ''*unassigned*)  var-list))
             (list 'set! (definition-variable token) (definition-value token)))
            (else token)))
    (set! new-body (list (cons 'let (cons var-list (map scan-body exp-body)))))
    (if (null? var-list)
      exp-body
      new-body)))

(define (lookup-variable-value var env)
  (let ((token (look-for-var var env)))
    (cond ((null? token) (error ";;M-Eval error: Unbound variable" var))
          ((eq? (cdr token) '*unassigned*) (error ";;M-Eval error: UNASSIGNED variable" var))
          (else (cdr token)))))

(define (set-variable-value! var val env)
  (let ((token (look-for-var var env)))
    (if (null? token)
      (error "M-Eval: Unbound variable -- SET!" var)
      (set-cdr! token val))))

;------------

(define (define-variable! var val env)
  (let ((token (look-for-var var env)))
    (if (null? token)
      (add-binding-to-frame! var val (first-frame env))
      (set-cdr! token val))))

(define (make-unbound var env)
  (define (scan frame)
    (cond ((or (null? frame) (null? (cdr frame)))
           (error "Unbound variable -- MAKE-UNBOUND!" var))
          ((eq? var  (caar frame))
           (set-car! (car frame) '())
           (set-cdr! (car frame) '()))
          (else (scan (cdr frame)))))
  (if (eq? env the-empty-environment)
    (error "Unbound variable -- MAKE-UNBOUND!" var))
  (let ((frame (first-frame env)))
    (scan frame)))
;-----------------

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

;-------------------

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
            (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   ))
    (display object)))

;-----------------

(define unless '(define (unless condition usual-value exceptional-value)
                  (if condition exceptional-value usual-value)))

(eval unless the-global-environment)

(define program '(begin
                   (define (cons x y)
                     (lambda (m) (m x y)))
                   (define (car z)
                     (z (lambda (p q) p)))
                   (define (cdr z)
                     (z (lambda (p q) q)))
                   (define (list-ref items n)
                     (if (= n 0)
                       (car items)
                       (list-ref (cdr items) (- n 1))))
                   (newline)
                   (display (car '(a b c)))))

(newline)
(display program)
(newline)
(eval program the-global-environment)
(newline)

(driver-loop)

