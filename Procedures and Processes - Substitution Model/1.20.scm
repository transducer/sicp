#lang scheme

;; Famous algorithm for computing the greatest common divisor: Euclid's Algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

;; 206 40
;; 40  6
;; 6   4
;; 4   2
;; 2   0
;; 2
;; => 4 remainder calculations when using applicative order.