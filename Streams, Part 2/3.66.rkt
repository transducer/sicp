#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; Examine the stream (pairs integers integers). Can you make any general 
;; comments about the order in which the pairs are placed into the stream? 
;; For example, about how many pairs precede the pair (1,100)? the pair 
;; (99,100)? the pair (100,100)? (If you can make precise mathematical 
;; statements here, all the better. But feel free to give more qualitative 
;; answers if you find yourself getting bogged down.) 

; Import from the book (after converting to Racket)
(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(display-stream
 (pairs (stream-cons 1 (stream-cons 2 (stream-cons 3 (stream-cons 4 (stream-cons 5 (stream-cons 6 '()))))))
        (stream-cons 1 (stream-cons 2 (stream-cons 3 (stream-cons 4 (stream-cons 5 (stream-cons 6 '()))))))))

; => (1 1)
;    (1 2)
;    (2 2)
;    (1 3)
;    (2 3)
;    (1 4)
;    (3 3)
;    (1 5)
;    (2 4)
;    (1 6)
;    (3 4)
;    (2 5)
;    (4 4)
;    (2 6)
;    (3 5)
;    (4 5)
;    (3 6)
;    (5 5)
;    (4 6)
;    (5 6)
;    (6 6)

; (1,100), car of pair is 1 gives second value of 2n-2 for n > 2 so the
;          198th pair.
; (99,100) One minus the length, so n(n+1)/2 -1 = 100(100+1)/2 -1 = 5049
; (100,100 n(n+1)/2 = 100(100+1)/2 = 5050

; Full formula from wqzhang solution is:

; N(i,j) = 2^(i-1)(max(1,2(j-i))+1)-2

; (Source: https://wqzhang.wordpress.com/2009/08/17/sicp-exercise-3-66/)
