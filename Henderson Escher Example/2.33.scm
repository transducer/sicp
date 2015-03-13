#lang scheme

;; Fill in the missing expressions to complete the following 
;; definitions of some basic list-manipulation operations as 
;; accumulations:

;; (define (map p sequence)
;;   (accumulate (lambda (x y) <??>) nil sequence))
;; (define (append seq1 seq2)
;;   (accumulate cons <??> <??>))
;; (define (length sequence)
;;   (accumulate <??> 0 sequence))

;; Use accumulate from the book:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; Define some necessary stuff:
(define nil '())
(define square (lambda (x) (* x x)))

;; Answer the question:
(define (map p sequence)
  (accumulate (lambda (f acc) (cons (p f) acc)) 
              nil sequence))

;; Testing:
(map square (list 1 2 3))          ;; => (1 4 9)


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;; Testing:
(append (list 1 2 3) (list 4 5 6)) ;; => (1 2 3 4 5 6)


(define (length sequence)
  (accumulate (lambda (f acc) (+ acc 1)) 0 sequence))

;; Testing:
(length (list 1 2 3 4 5 5))        ;; => 6
