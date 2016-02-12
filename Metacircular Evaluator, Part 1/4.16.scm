#lang scheme

;; In this exercise we implement the method just described for interpreting internal 
;; definitions. We assume that the evaluator supports let (see exercise 4.6).

;; a.  Change lookup-variable-value (section 4.1.3) to signal an error if the value 
;; it finds is the symbol *unassigned*.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? val '*unassigned*)
                 (error "value is *unassigned*" var)
                 val)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;; b.  Write a procedure scan-out-defines that takes a procedure body and returns an 
;; equivalent one that has no internal definitions, by making the transformation 
;; described above.

(define (scan-out-defines expr)
  (let ((vars (cadr expr))
        (body (cddr expr)))
    (make-lambda vars
                 ; loop over body,
                 ; store all definition names and bodies of the defines
                 ; once finished looping transform those into lets
                 ; where the rest is added to the body
                 (let body-transform ((body-elements body)
                                      (definition-names '())
                                      (definition-bodies '())
                                      (rest-of-body '()))
                   (if (null? body-elements)
                     (transform-define-into-let definition-names 
                                                definition-bodies 
                                                rest-of-body)
                     (let ((current-element (car body-elements)))
                       (if (tagged-list? current-element 'define)
                         (body-transform (cdr body-elements)
                                         (cons (get-definition-name current-element) 
                                               definition-names)
                                         (cons (get-definition-body current-element) 
                                               definition-bodies)
                                         rest-of-body)
                         (body-transform (cdr body-elements)
                                         definition-names
                                         definition-bodies
                                         (cons current-element rest-of-body)))))))))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (get-definition-name expr)
  (cadr expr))

(define (get-definition-body expr)
  (caddr expr))

(define (transform-define-into-let definition-names definition-bodies rest-of-body)
  (list (append (list 'let (make-unassigned-vars definition-names))
              ; We need to flatten the lists once
              (append (make-sets definition-names definition-bodies)
              rest-of-body))))

(define (make-unassigned-vars vars)
  (let aux ((var-elements vars)
            (unassigned-vars '()))
    (if (null? var-elements)
      unassigned-vars
      (aux (cdr var-elements)
           (cons (list (car var-elements) ''*unassigned*) unassigned-vars)))))

(define (make-sets vars vals)
  (let aux ((var-elements vars)
            (val-elements vals)
            (sets '()))
    (if (null? var-elements)
      sets
      (aux (cdr var-elements)
           (cdr val-elements)
           (cons (list 'set! (car var-elements) (car val-elements)) sets)))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; testing
(scan-out-defines '(lambda (a b)
                     (define u 'u)
                     (define v 'v)
                     'e1))

; => (lambda (a b)
;      (let ((u '*unassigned*)
;            (v '*unassigned*))
;        (set! u 'u)
;        (set! v 'v)
;        'e1))

;; c.  Install scan-out-defines in the interpreter, either in make-procedure or in 
;; procedure-body (see section 4.1.3). Which place is better? Why?

; Install it into make-procedure. Feels better semantically (something is made).

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

