#lang racket

(require racket/include)
(include "racket-stream-extra.rkt")

;; With power series represented as streams of coefficients as in 
;; exercise 3.59, adding series is implemented by add-streams. Complete 
;; the definition of the following procedure for multiplying series:

; (define (mul-series s1 s2)
;   (cons-stream <??> (add-streams <??> <??>)))

;; You can test your procedure by verifying that sin2 x + cos2 x = 1, using 
;; the series from exercise 3.59. 


; Definitions needed from exercise 3.59:

(define (integrate-series s)
  (stream-map / s integers))

(define cosine-series
  (stream-cons 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

; ... and exercise 3.55:

(define (partial-sums s)
  (add-streams s 
               (stream-cons 0 
                            (partial-sums s))))

; Solve:


(define (mul-series s1 s2)
  (stream-cons 
   (* (stream-first s1) (stream-first s2))
   (add-streams 
    (scale-stream (stream-rest s2) (stream-first s1))
    (mul-series (stream-rest s1) s2))))

; Testing

(define one (add-streams (mul-series sine-series sine-series)
                         (mul-series cosine-series cosine-series)))
(stream-ref one 0)
;= > 1
(stream-ref one 1)
;= > 0
(stream-ref one 2)
;= > 0
(stream-ref one 3)
;= > 0

