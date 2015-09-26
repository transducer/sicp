#lang racket

(require racket/include)
(include "racket-stream-extra.rkt")

;; Define a procedure partial-sums that takes as argument a stream S and 
;; returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... 
;; For example, (partial-sums integers) should be the stream 
;; 1, 3, 6, 10, 15, .... 

; The idea...

; 1  2  3  4  5 ...
;    1  2  3  4 ...
;       1  2  3 ...
;          1  2 ...
;             1 ...
;------------------- +
; 1  3  6  10 15...

; The implementation...

(define (partial-sums s)
  (stream-cons
   (stream-first s)
   (add-streams (stream-rest s) 
                (partial-sums s))))


; Testing

(display-stream (partial-sums integers))
; => 1
;    3
;    6
;    10
;    15
;    21