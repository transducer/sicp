#lang scheme

(define (largest-squares-sum a b c)
  (define (square x) (* x x))
  (cond ((and (< a b) (< a c)) (+ (square b) (square c)))
        ((and (< c b) (< c a)) (+ (square b) (square c)))
        ((and (< b a) (< b c)) (+ (square b) (square c)))))

(largest-squares-sum 4 5 6)

