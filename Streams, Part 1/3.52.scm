#lang racket

(require racket/include)
(include "stream.rkt")

;; Consider the sequence of expressions

; (define sum 0)
; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; (define y (stream-filter even? seq))
; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                          seq))
; (stream-ref y 7)
; (display-stream z)

;; What is the value of sum after each of the above expressions is 
;; evaluated? 

(define sum 0)
; => sum: 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
; => sum: 1

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))
; => sum: 6

(define y 
  (stream-filter even? seq))
; => sum: 100

(define z 
  (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
; => sum: 136

(stream-ref y 7)
; => 56

(display-stream z)
; => 210


;; What is the printed response to evaluating the stream-ref and 
;; display-stream expressions? 

(stream-ref y 7)
; => 56

(display-stream z)
; => 10, 15, 45, 55, 105, 120, 190, 210


;; Would these responses differ if we had implemented (delay <exp>) simply 
;; as (lambda () <exp>) without using the optimization provided by
;; memo-proc ? Explain. 

; Yes. Because the accumulator contains state and there is caching.
