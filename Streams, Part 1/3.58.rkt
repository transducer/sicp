#lang racket

(require racket/include)
(include "racket-stream-extra.rkt")

;; Give an interpretation of the stream computed by the following 
;; procedure:

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; (Quotient is a primitive that returns the integer quotient of two 
;; integers.) 

; => It returns a stream of the fraction in base radix of num/den.

;; What are the successive elements produced by (expand 1 7 10)?

(display-stream (expand 1 7 10))
; => 1
;    4
;    2
;    8

;; What is produced by (expand 3 8 10) ? 

(display-stream (expand 3 8 10))
; => 3
;    7
;    5
;    0
;    0
;    .
;    .
;    .
