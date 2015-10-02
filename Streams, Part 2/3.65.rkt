#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; Use the series

; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...

;; to compute three sequences of approximations to the natural logarithm of 
;; 2, in the same way we did above for. How rapidly do these sequences 
;; converge? 

; First create the series:

(define alternating-ones-and-minus-ones
  (stream-cons 1
               (stream-cons -1
                            alternating-ones-and-minus-ones)))
(define ln-series
  (stream-map / alternating-ones-and-minus-ones integers))

; And use partial-sums of exercise 3.55 to create the stream that
; approximates ln 2 on each subsequent element.

(define (partial-sums s)
  (add-streams s
               (stream-cons 0
                            (partial-sums s))))

(define ln-stream
  (partial-sums ln-series))

; Use stream accelerators from the book (adjusted for Racket):

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first
              (make-tableau transform s)))

; First normally:

(display-stream ln-stream)
; => 1
;    1/2
;    5/6
;    7/12
;    47/60
;    37/60
;    319/420
;    533/840
;    1879/2520

;    8th element is 

(exact->inexact 
 (stream-ref ln-stream 8))
; => 0.7456349206349207

; Then accelerated once:

(display-stream (euler-transform ln-stream))
; => 7/10
;    29/42
;    25/36
;    457/660
;    541/780
;    97/140
;    9901/14280
;    33181/47880
;    1747/2520

;    8th element is

(exact->inexact 
 (stream-ref (euler-transform ln-stream) 8))
; => 0.6932539682539682

; Then accelerate the accelerated stream:

(display-stream (accelerated-sequence euler-transform
                                      ln-stream))

; => 1
;    7/10
;    165/238
;    380522285/548976276
;    755849325680052062216639661/1090460049411856348776491380
;    (...)

(exact->inexact 
 (stream-ref (accelerated-sequence euler-transform ln-stream) 8))
; => 0.6931471805599435 

; Wolfram gives 
;    0.6931471805599453...

; First one converges slowly, second sequence quite fast and third one
; rather amazingly fast. I am not able to give a quantitative answer, 
; neither can I find one yet.
