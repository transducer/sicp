#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; It would be nice to be able to generate streams in which the pairs 
;; appear in some useful order, rather than in the order that results from 
;; an ad hoc interleaving process. We can use a technique similar to the 
;; merge procedure of exercise 3.56, if we define a way to say that one pair 
;; of integers is ``less than'' another. One way to do this is to define a 
;; ``weighting function'' W(i,j) and stipulate that (i1,j1) is less than 
;; (i2,j2) if W(i1,j1) < W(i2,j2). Write a procedure merge-weighted that is 
;; like merge, except that merge-weighted takes an additional argument weight, 
;; which is a procedure that computes the weight of a pair, and is used to 
;; determine the order in which elements should appear in the resulting merged 
;; stream. Using this, generalize pairs to a procedure weighted-pairs that 
;; takes two streams, together with a procedure that computes a weighting 
;; function, and generates the stream of pairs, ordered according to weight.  

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

;; Use your procedure to generate

;; a. the stream of all pairs of positive integers (i,j) with i < j ordered 
;; according to the sum i + j

(define (positive-integers-ordered-by-sum-i-plus-j)
  (define (weight pair)
    (+ (car pair) (cadr pair)))
  (weighted-pairs integers integers weight))

(display-stream (positive-integers-ordered-by-sum-i-plus-j))
; => (1 1)
;    (1 2)
;    (1 3)
;    (1 4)
;    (1 5)
;    (1 6)
;    (1 7)
;    (1 8)
;    (1 9)
;    (1 10)

;; b. the stream of all pairs of positive integers (i,j) with i < j, where 
;; neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered 
;; according to the sum 2 i + 3 j + 5 i j.

(define (positive-integers-ordered-awesomely)
  (define (weight pair)
    (let ((i (car pair)) 
          (j (cadr pair)))
      (+ (* 2 i) (* 3 j) (* 5 i j))))
  
  (let ((integers-not-disivible-by-2-3-or-5
         (stream-filter 
          (lambda (e) (not (or (= (remainder e 2) 0)
                               (= (remainder e 3) 0)
                               (= (remainder e 5) 0))))
          integers)))
    (weighted-pairs integers-not-disivible-by-2-3-or-5 
                    integers-not-disivible-by-2-3-or-5
                    weight)))

(display-stream (positive-integers-ordered-awesomely))
; => (1 1)
;    (1 7)
;    (1 11)
;    (1 13)
;    (1 17)
;    (1 19)
;    (1 23)
;    (1 29)
;    (1 31)
;    (7 7)
