#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; Write a procedure triples that takes three infinite streams, S, T, and 
;; U, and produces the stream of triples (Si,Tj,Uk) such that i <= j <= k. 
;; Use triples to generate the stream of all Pythagorean triples of 
;; positive integers, i.e., the triples (i,j,k) such that i < j and 
;; i^2 + j^2 = k^2. 

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(define (triples s t u)
  (stream-cons
   (list (stream-first s) (stream-first t) (stream-first u))
   (interleave
    (stream-map 
     (lambda (x) (cons (stream-first s) x))
     (stream-rest (pairs t u)))
    (triples (stream-rest s) (stream-rest t) (stream-rest u)))))

(define (pythagorean-triples)
  (define (square x) (* x x))
  (stream-filter 
   (lambda (t) (= (+ (square (car t)) (square (cadr t))) 
                  (square (caddr t))))
   (triples integers integers integers)))


; Testing

(display-stream (pythagorean-triples))
; => (3 4 5)
;    (6 8 10)
;    (5 12 13)
;    (9 12 15)
;    (8 15 17)
;    (12 16 20)
