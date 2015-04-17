#lang scheme

;; Define a procedure unique-pairs that, given an integer n, 
;; generates the sequence of pairs (i,j) with 1< j< i< n. 
;; Use unique-pairs to simplify the definition of 
;; prime-sum-pairs given above. 

;; prime-sum-pairs:

(define nil '())

(define (enumerate-interval low high) 
  (if (> low high) 
      nil 
      (cons low (enumerate-interval (+ low 1) high)))) 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


;; Answer the question by extracting the unique-pairs from
;; the prime-sum-pairs function.

(define (unique-pairs n) 
  (flatmap (lambda (i)  
             (map (lambda (j) (list i j)) 
                  (enumerate-interval 1 (- i 1)))) 
           (enumerate-interval 1 n))) 

;; Testing:
(unique-pairs 4) 
;; => ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))


(define (prime-sum-pairs-simplified n) 
  (map make-pair-sum 
       (filter prime-sum? (unique-pairs n)))) 

;; Testing:
(prime-sum-pairs-simplified 4) 
;; => ((2 1 3) (3 2 5) (4 1 5) (4 3 7))
