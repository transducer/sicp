#lang scheme

;; Write filtered-accumulate as a procedure. Show how to express 
;; the following using filtered-accumulate:

(define (accumulate-filter 
         combiner null-value term a next b filter)
  (cond 
    ((> a b) null-value)
    ((filter a) (combiner (term a) (accumulate-filter
                                    combiner
                                    null-value
                                    term
                                    (next a)
                                    next b
                                    filter)))
    (else (combiner null-value (accumulate-filter
                                combiner
                                null-value
                                term
                                (next a)
                                next b
                                filter)))))

;; a) The sum of the squares of the prime numbers in the interval
;;    a to b (assuming that you have a prime? predicate already 
;;    written)

(define (square x) (* x x))
(define (inc x) (+ x 1))

;; Prime from the book:
(define (prime? n)
  (define (smallest-div n) 
    (define (divides? a b)
      (= 0 (remainder b a)))
    (define (find-div n test)
      (cond ((> (square test) n) n) 
            ((divides? test n) test)
            (else (find-div n (+ test 1))))) 
    (find-div n 2))
  
  (if (= n 1) 
      false 
      (= n (smallest-div n))))

(define (sum-of-prime-squares a b)
  (accumulate-filter + 0 square a inc b prime?))

(sum-of-prime-squares 1 5) ;; => 38

;; b) The product of all the positive integers less than n that 
;;    are relatively prime to n (i.e., all positive integers 
;;    i < n such that GCD(i,n) = 1)

;; Euclidian algorithm
(define (gcd m n) 
  (cond ((< m n) (gcd n m)) 
        ((= n 0) m) 
        (else (gcd n (remainder m n)))))

(define (relative-prime? m n) 
  (= (gcd m n) 1))

;; With help of solution: define filter inside of function!
(define (product-of-relative-primes n) 
  (define (filter x)
    (relative-prime? x n)) 
  (accumulate-filter * 1 identity 1 inc n filter))

(product-of-relative-primes 10) ;; => 189

