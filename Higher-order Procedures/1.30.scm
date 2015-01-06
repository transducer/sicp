#lang scheme

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; The sum procedure above generates a linear recursion. 
;; The procedure can be rewritten so that the sum is performed 
;; iteratively. Show how to do this by filling in the missing 
;; expressions in the following definition.

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Test the results
(define (identity x) x)
(define (inc x) (+ x 1))
(define (sum-integers a b)
  (sum-iter identity a inc b))

(sum-integers 1 10) ;; => 55

