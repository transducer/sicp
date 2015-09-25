#lang racket

(require racket/include)
(include "stream.rkt")

;; In order to take a closer look at delayed evaluation, we will use the 
;; following procedure, which simply returns its argument after printing it:

; (define (show x)
;   (display-line x)
;   x)

;; What does the interpreter print in response to evaluating each expression
;; in the following sequence?

; (define x (stream-map show (stream-enumerate-interval 0 10)))
; (stream-ref x 5)
; (stream-ref x 7)

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
; => 0 1 2 3 4 5 6 7 8 9

(stream-ref x 5)
; => 5

(stream-ref x 7)
; => 7
