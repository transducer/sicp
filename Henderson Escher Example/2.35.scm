#lang scheme

;; Redefine count-leaves from section 2.2.2 as an accumulation:
;;
;; (define (count-leaves t)
;;   (accumulate <??> <??> (map <??> <??>)))

;; Using accumulate from the book:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; count-leaves looked as follows:
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 2 (list 3 4))) ;; => 4

;; Redefine in terms of accumulate and map:
(define (count-leaves2 x)
  (accumulate + 0 (map (lambda (l)
                         (if (pair? l)
                             (count-leaves2 l)
                             1))
                       x)))

(count-leaves2 (list 1 2 (list 3 4))) ;; => 4




