#lang scheme

;; Helper functions:

(define (square a) (* a a))
(define (1+ a) (+ a 1))
;; Today we are going to smash the idea that LISP is
;; similar to Pascal or BASIC.

;; Here are some programs that demonstrate that:

;; Sum of the integers from a to b.

(define (sum-int a b) 
  (if (> a b) 
      0
      (+ a (sum-int (+ a 1) b))))

(sum-int 1 10) ;; => 55

;; Sum of squares a to b.

(define (sum-sq a b)
  (if (> a b) 
      0
      (+ (square a)
         (sum-sq (+ a 1) b))))

(sum-sq 1 10) ;; => 385

;; Almost identical programs. When you see yourself
;; writing something down more than once start thinking.

;; Another one, interestingly enough these sums tend to
;; converge to values like pi over 8.
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(* 8 (pi-sum 1 1000)) ;; ~3,14.. 

;; Another identical one. We have to come up with some
;; kind of abstraction. Common patterns of usages. See:

;; (define (<name> a b)
;;   (if (> a b) 
;;     0
;;    (+ (<term> a)
;;        (<name> (<next> a) b))))

;; In LISP we can give the common pattern a name.

;; There is nothing very special about numbers. Now we
;; will show some procedural arguments. Later on we will
;; show something that is not like that.

(define (sum term a next b) ;; notice term and next
  (if (> a b)
      0
      (+ (term a)
         (sum term 
              (next a)
              next
              b))))

  
;; Iterative implementation of sum:

(define (sum-iter term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (* (term j) ans))))
  (iter a 0))


;; Write down original program as instances of sum.

(define (sum-int2 a b)
  (define (identity x) x)
  (sum identity a 1+ b))

(sum-int2 1 10) ;; => 55

(define (sum-sq2 a b)
  (sum square a 1+ b))
 
(sum-sq2 1 10) ;; => 385

(define (pi-sum2 a b)
  (sum (lambda (i) (/ 1 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

(* 8 (pi-sum2 1 1000)) ;; => ~3,14.. 

;; The procedure that takes a procedural argument
;; allows us to divide a problem.
