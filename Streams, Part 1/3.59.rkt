#lang racket

(require racket/include)
(include "racket-stream-extra.rkt")

;; In section 2.5.3 we saw how to implement a polynomial arithmetic system 
;; representing polynomials as lists of terms. In a similar way, we can 
;; work with power series, such as

; e^x = 1 + x + x^2/2 + x^3/3*2 + x^4/4*3*2 + ...,

; cos x = 1 - x^2/2 + x^4/4*3*2 - ...,

; sin x = x - x^3/3*2 + x^5/5*4*3*2 - ...,

;; represented as infinite streams. We will represent the series 
;; a0 + a1 x + a2 x^2 + a3 x^3 + ··· as the stream whose elements are the 
;; coefficients a0, a1, a2, a3, ...

;; a. The integral of the series a0 + a1 x + a2 x^2 + a3 x^3 + ··· is the 
;;    series

;     c + a0x + 1/2 a1x^2 + 1/3 a2x^3 + 1/4 a3x^4 + ...

;;    where c is any constant. Define a procedure integrate-series that 
;;    takes as input a stream a0, a1, a2, ... representing a power series 
;;    and returns the stream a0, (1/2)a1, (1/3)a2, ... of coefficients of 
;;    the non-constant terms of the integral of the series. (Since the 
;;    result has no constant term, it doesn't represent a power series; 
;;    when we use integrate-series, we will cons on the appropriate 
;;    constant.)

(define (integrate-series s)
  (stream-map / s integers))


; Testing

; (display-stream (integrate-series ones))

; => 1
;    1/2
;    1/3
;    1/4
;    1/5
;    1/6
;    1/7


;; b. The function x -> e^x is its own derivative. This implies that e^x
;;    and the integral of ex are the same series, except for the constant 
;;    term which is e0 = 1. Accordingly, we can generate the series for ex
;;    as

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

;;    Show how to generate the series for sine and cosine, starting from 
;;    the facts that the derivative of sine is cosine and the derivative of 
;;    cosine is the negative of sine:

;     (define cosine-series
;       (cons-stream 1 <??>))
;     (define sine-series
;       (cons-stream 0 <??>))

(define cosine-series
  (stream-cons 1 (scale-stream (integrate-series sine-series) -1)))
 
(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))


; Testing

(display-stream cosine-series)
; => 1
;    0
;    -1/2
;    0
;    1/24
;    0
;    -1/720
;    0
;    1/40320


(display-stream sine-series)
; => 0
;    1
;    0
;    -1/6
;    0
;    1/120
;    0
;    -1/5040
;    0
;    1/362880
