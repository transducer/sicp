#lang scheme

;; a) The sum procedure is only the simplest of a vast number of 
;;    similar abstractions that can be captured as higher-order 
;;    procedures. Write an analogous procedure called product \
;;    that returns the product of the values of a function at 
;;    points over a given range. Show how to define factorial in 
;;    terms of product. 

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; Test the result
(define (identity x) x)
(define (inc x) (+ x 1))

(product identity 1 inc 10) ;; => 3628800

;; Define factorial
(define (factorial x) (product identity 1 inc x))

;; Test the result
(factorial 10) ;; => 3628800

;; Also use product to compute approximations to pi with the
;; formula:
;;  pi    2 * 4 * 4 * 6 * 6 * 8
;;  -- =  ---------------------
;;  4     3 * 3 * 5 * 5 * 7 * 7

(define next-addition 0)

(define (incrementby0thenby2 x)
  (cond 
    [(= next-addition 0)
     (set! next-addition 2)
     (+ x 0)]
    [(= next-addition 2)
     (set! next-addition 0)
     (+ x 2)]))

(define precision 10) 
(define wallis-denominator 
  (product identity 2 incrementby0thenby2 precision))
(define wallis-divisor 
  (* 3 (product identity 3 incrementby0thenby2 precision)))

(define pi (* 4 (/ wallis-denominator wallis-divisor)))

;; Testing
pi ;; => precision 10 = 5,50
   ;;    precision 20 = 10,73 
   ;; So, does not work...

;; Looking at solutions shows the following cleaner idea:
(define (pi-term n) 
  (if (even? n) 
      (/ (+ n 2) (+ n 1)) 
      (/ (+ n 1) (+ n 2))))

(* 4 (product pi-term 1 inc 100)) ;; => 3.15


;; b) If your product procedure generates a recursive process, 
;;    write one that generates an iterative process. If it 
;;    generates an iterative process, write one that generates 
;;    a recursive process. 

;; Ours generated an iterative proces. Now recursive:

(define (product-recursive term a next b)
  (if (> a b)
        1
        (* (term a)
           (product-recursive term (next a) next b))))

;; Testing:
(product-recursive identity 1 inc 10) ;; => 3628800