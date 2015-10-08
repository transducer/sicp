#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; Numbers that can be expressed as the sum of two cubes in more than one 
;; way are sometimes called Ramanujan numbers, in honor of the 
;; mathematician Srinivasa Ramanujan. Ordered streams of pairs provide an 
;; elegant solution to the problem of computing these numbers. To find a 
;; number that can be written as the sum of two cubes in two different ways,
;; we need only generate the stream of pairs of integers (i,j) weighted 
;; according to the sum i^3 + j^3 (see exercise 3.70), then search the 
;; stream for two consecutive pairs with the same weight. Write a procedure 
;; to generate the Ramanujan numbers. The first such number is 1,729. What 
;; are the next five?

; Procedures from previous exercise:

(define (merge-weighted s1 s2 weight) 
  (cond ((stream-empty? s1) s2) 
        ((stream-empty? s2) s1) 
        (else 
         (let ((s1car (stream-first s1)) 
               (s2car (stream-first s2))) 
           (cond ((< (weight s1car) (weight s2car)) 
                  (stream-cons s1car (merge-weighted (stream-rest s1) 
                                                     s2 
                                                     weight))) 
                 ((> (weight s1car) (weight s2car)) 
                  (stream-cons s2car (merge-weighted s1 
                                                     (stream-rest s2)
                                                     weight))) 
                 (else 
                  (stream-cons s1car (merge-weighted (stream-rest s1) 
                                                     (stream-rest s2)
                                                     weight))))))))

(define (weighted-pairs s t weight)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (weighted-pairs (stream-rest s) (stream-rest t) weight)
    weight)))

; Create stream for Ramanujan numbers

(define (weight pair)
  (+ (expt (car pair) 3) (expt (cadr pair) 3)))

(define ramanujan-numbers
  (weighted-pairs integers integers weight))

; Search the stream for two consecutive pairs with the same weight
; (and get the weight)

(define (ramanujan-stream s)
  (let* ((rest-of-stream (stream-rest s))
         (first-of-rest-of-stream (stream-first rest-of-stream))
         (w1 (weight (stream-first s)))
         (w2 (weight first-of-rest-of-stream)))
    (if (= w1 w2)
        (stream-cons w1 (ramanujan-stream rest-of-stream))
        (ramanujan-stream rest-of-stream))))

(display-stream (ramanujan-stream ramanujan-numbers))
; =>
