#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; Modify the pairs procedure so that (pairs integers integers) will 
;; produce the stream of all pairs of integers (i,j) (without the condition 
;; i < j). Hint: You will need to mix in an additional stream. 

; Import from the book (after converting to Racket)
(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

; Interleave the original produced pairs with the opposite ordered 
; result. Used SICP solutions from the Scheme wiki
; (http://community.schemewiki.org/?sicp-ex-3.67) to help me see it.
(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-first s) x)) (stream-rest t))
     (pairs (stream-rest s) (stream-rest t)))
    (stream-map (lambda (x) (list x (stream-first t))) (stream-rest s)))))

(display-stream
 (pairs (stream-cons 1 (stream-cons 2 (stream-cons 3 '())))
        (stream-cons 1 (stream-cons 2 (stream-cons 3 '())))))

; => (1 1)
;    (1 2)
;    (2 1)
;    (2 2)
;    (3 1)
;    (1 3)
;    (2 3)
;    (3 2)
;    (3 3)

