#lang scheme

;; Complete the following definitions of reverse (exercise 2.18) 
;; in terms of fold-right and fold-left from exercise 2.38: 

;; (define (reverse sequence)
;;   (fold-right (lambda (x y) <??>) nil sequence))
;; (define (reverse sequence)
;;   (fold-left (lambda (x y) <??>) nil sequence))

;; Define fold-left and fold-right

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define nil '())

(define (reverse-with-fold-right sequence)
  (fold-right (lambda (x y) (append y (if (pair? x) (car x) (cons x '())))) nil sequence))

(define (reverse-with-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; Testing:
(reverse-with-fold-right (list 1 2 3 4 5)) ;; => (5 4 3 2 1)
(reverse-with-fold-left (list 1 2 3 4 5))  ;; => (5 4 3 2 1)
