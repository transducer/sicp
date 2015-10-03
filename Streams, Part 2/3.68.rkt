#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; Louis Reasoner thinks that building a stream of pairs from three parts 
;; is unnecessarily complicated. Instead of separating the pair (S0,T0) 
;; from the rest of the pairs in the first row, he proposes to work with 
;; the whole first row, as follows:

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-first s) x))
               t)
   (pairs (stream-rest s) (stream-rest t))))

;; Does this work? Consider what happens if we evaluate (pairs integers 
;; integers) using Louis's definition of pairs.

; No, an infinite loop will result. This is because pairs call interleave
; who calls pairs again. There is no delayed evaluation which can be forced.

(define (pairs-with-display s t)
  (display "Entering interleave") (newline)
  (interleave
   (stream-map (lambda (x) (list (stream-first s) x))
               t)
   (display "Entering pairs") (newline)
   (pairs-with-display (stream-rest s) (stream-rest t))))

(display-stream (pairs-with-display integers integers))
; => Entering interleave
;    Entering pairs
;    Entering interleave
;    Entering pairs
;    Entering interleave
;    Entering pairs
;    Entering interleave
;    Entering pairs
