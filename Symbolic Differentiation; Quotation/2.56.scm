#lang scheme

;; Show how to extend the basic differentiator to handle more kinds of 
;; expressions. For instance, implement the differentiation rule

;; d(u^n)/dx = nu^(n-1)(du/dx)

;; by adding a new clause to the deriv program and defining appropriate 
;; procedures exponentiation?, base, exponent, and make-exponentiation. 
;; (You may use the symbol ** to denote exponentiation.) Build in the rules
;; that anything raised to the power 0 is 1 and anything raised to the power
;; 1 is the thing itself. 

;; "Import" procedures from the book:
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; The adjusted procedures:
(define (exponentiation? x) 
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponentiation a b)
  (cond ((=number? b 0) 1)
        ((=number? b 1) a)
        ((=number? a 1) 1)
        (else (list '** a b))))

;; Add a helper for subtracting
(define (sub1 x)
  (if (number? x) (- x 1)
      (list x '- '1)))

;; Implement the extra differentation rule:
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation 
                         (base exp)
                         (sub1 (exponent exp))))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Testing:
(deriv '(** x 2) 'x)
;; => (* 2 x)

(deriv '(** x 20) 'x)
;; => (* 20 (** x 19))

(deriv '(** 1 20) 'x)
;; => 0

(deriv '(** 1 x) 'x)
;; => 0, should be 1?

(deriv '(* 2 (** x 10)) 'x)
;; => (* 2 (* 10 (** x 9)))

