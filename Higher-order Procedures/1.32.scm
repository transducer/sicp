#lang scheme

;; a) Show that sum and product (exercise 1.31) are both special 
;;    cases of a still more general notion called accumulate that
;;    combines a collection of terms, using some general 
;;    accumulation function:

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (inc x) (+ x 1))
(define (identity x) x)

(define (sum x) (accumulate + 0 identity 0 inc x))
(define (product x) (accumulate * 1 identity 1 inc x))

;; Testing

(sum 10)     ;; => 55
(product 10) ;; => 3628800


;; b) From iterative to recursive.

(define (accumulate-recursive combiner null-value term a next b)
    (if (> a b) 
        null-value
        (combiner (term a)
         (accumulate-recursive 
          combiner null-value term (next a) next b))))

(define (sum-recursive x) 
  (accumulate-recursive + 0 identity 0 inc x))
(define (product-recursive x) 
  (accumulate-recursive * 1 identity 1 inc x))

(sum-recursive 10)     ;; => 55
(product-recursive 10) ;; => 3628800