; As we confront increasingly complex problems, we will find that Lisp, or 
; indeed any fixed programming language, is not sufficient for our needs. 
; We must constantly turn to new languages in order to express our ideas 
; more effectively. Establishing new languages is a powerful strategy for 
; controlling complexity in engineering design; we can often enhance our 
; ability to deal with a complex problem by adopting a new language that 
; enables us to describe (and hence to think about) the problem in a 
; different way, using primitives, means of combination, and means of 
; abstraction that are particularly well suited to the problem at hand.

; (...)

; Metalinguistic abstraction -- establishing new languages -- plays an 
; important role in all branches of engineering design. It is particularly 
; important to computer programming, because in programming not only can we
; formulate new languages but we can also implement these languages by 
; constructing evaluators. An evaluator (or interpreter) for a programming 
; language is a procedure that, when applied to an expression of the 
; language, performs the actions required to evaluate that expression.

; It is no exaggeration to regard this as the most fundamental idea in 
; programming:

;   The evaluator, which determines the meaning of expressions in a 
;   programming language, is just another program. 

; To appreciate this point is to change our images of ourselves as 
; programmers. We come to see ourselves as designers of languages, rather 
; than only users of languages designed by others.

; In fact, we can regard almost any program as the evaluator for some 
; language.

; (...)

; We now embark on a tour of the technology by which languages are 
; established in terms of other languages.

;  -- SICP chapter 4


; METALINGUISTIC ABSTRACTION, PART 1

; Evaluating Lisp in Lisp. Evaluation is a process, so it is appropriate to
; describe the evaluation process using Lisp, which is a tool is our tool
; for describing processes.

; "A certain amount of mysticism will be appearing that may be disturbing
;  and cause trouble in your minds." -- Gerald Jay Sussman

; The essence of the evaluation process, a basic cycle in which expressions
; to be evaluated in environments are reduced to procedures to be applied 
; to arguments, which in turn are reduced to new expressions to be 
; evaluated in new environments, and so on, until we get down to symbols, 
; whose values are looked up in the environment, and to primitive 
; procedures, which are applied directly. 

; This evaluation cycle will be emobided in two critical procedures:
; eval and apply.

; EVAL and APPLY on four blackboards:

; "Like every interesting procedure, it is a case analysis.

; "The program I am going to write on the blackboard is dirty, ugly, 
;  disgusting. Not the way I would write it as a professional. It is 
;  written with concrete syntax (lots of CARs and CDRs). That is on purpose
;  so it fits on the blackboard. I am not using long names like I normally
;  use. I don't want you writing programs like this. This is purely for an
;  effect." -> It is result of compiling clearer written code.

; Definition and assignment are not included for mathemathical reasons
; (they are not necessary) and they take up more space.

(define eval 
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cadr exp))
          ((eq? (car exp) 'lambda)
           (list 'closure (cdr exp) env)) ; 'closure description of type of
          ((eq? (car exp) 'cond)          ; compound thing
           (evcond (cdr exp) env))
          (else (apply (eval car exp) env)
                (evlist (cdr exp) env)))))

(define apply
  (lambda (proc args)
    (cond ((primitive? proc)
           (apply-primop proc args))       ; go to machine language
           ((eq? (car proc) 'closure)      ; interesting things follow: 
            (eval (cadadr proc)            ; glue which combines primitives
                  (bind (caadr proc)       ; together
                          args
                          (cadar proc))))
           (else error))))

(define evlist
  (lambda (l env)
    (cond ((eq? l '()) '())
          (else
           (cons (eval (car l) env)
                 (evlist (cdr l) env))))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (eval (caar clauses) env))
           (evcond (cdr clauses) env))
          (else
           (eval (cadar clauses) env)))))

(define bind                                ; bind creates new environment
  (lambda (vars vals env)                   ; structure by consing up new
    (cons (pair-up vars vals)               ; frame to existing environment
          env)))                            ; structure

(define pair-up
  (lambda (vars vals)
    (cond
      ((eq? vars '())
       (cond ((eq? vals '()) '())
             (else (error "Too many arguments"))))
      ((eq? vals '()) (error "Too few arguments"))
      (else
       (cons (cons (car vars)
                   (car vals))
             (pair-up (cdr vars)
                      (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error "Unbound variable"))
          (else
           ((lambda (vcell)
              (cond ((eq? vcell '())
                     (lookup sym
                             (cdr env)))
                    (else (cdr vcell))))
            (assq sym (car env)))))))

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist))
           (car alist))
          (else
           (assq sym (cdr alist))))))

