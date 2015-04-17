#lang scheme

;; Write a procedure to find all ordered triples of distinct 
;; positive integers i, j, and k less than or equal to a 
;; given integer n that sum to a given integer s. 

;; Define helpers:

(define (filter predicate sequence) 
  (cond ((null? sequence) nil) 
        ((predicate (car sequence)) 
         (cons (car sequence)  
               (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence))))) 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define nil '())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high) 
  (if (> low high) 
      nil 
      (cons low (enumerate-interval (+ low 1) high))))

;; Answer the question:

;; Use unique-pairs from previous and append each number 
;; upto n to the front on every unique pair.
(define (unique-triples n)
  (flatmap
   (lambda (a)
     (map (lambda (b) (cons a b))
          (flatmap (lambda (i)  
             (map (lambda (j) (list i j)) 
                  (enumerate-interval 1 (- i 1)))) 
           (enumerate-interval 1 (- a 1)))))
   (enumerate-interval 1 n)))

(define (find-ordered-triples n s)
  ;; only keep the triples that add up to 10:
  (filter (lambda (triple) (= (accumulate + 0 triple) s))
          (unique-triples n)))

;; Testing:
(find-ordered-triples 10 10) 
;; => ((5 3 2) (5 4 1) (6 3 1) (7 2 1))
