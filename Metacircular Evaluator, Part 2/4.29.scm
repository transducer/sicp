#lang scheme

;; Exhibit a program that you would expect to run much more slowly without memoization 
;; than with memoization. 

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fib (- n 1) (fib (- n 2))))))

;; Also, consider the following interaction, where the id procedure is defined as in 
;; exercise 4.27 and count starts at 0:

; (define (square x)
;   (* x x))
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval value:
; <response>
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; <response>

;; Give the responses both when the evaluator memoizes and when it does not. 

