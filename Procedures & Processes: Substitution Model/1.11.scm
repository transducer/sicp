#lang scheme

; f(n) = n if n < 3 and 
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n > 3

; Recursive process f
(define (f n) 
  (if (< n 3) n 
      (+ (f (- n 1)) 
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 1) ; => 1  
(f 2) ; => 2
(f 3) ; => 4
(f 4) ; => 11
(f 5) ; => 25
(f 6) ; => 59

; Iterative process g
(define (g n)
  (g-iter 0 1 2 n))
(define (g-iter a b c count)
  (if (= count 0) a
      (g-iter b c (+ (* 3 a) (* 2 b) c) (- count 1))))

(g 1) ; => 1
(g 2) ; => 2
(g 3) ; => 4
(g 4) ; => 11
(g 5) ; => 25
(g 6) ; => 59
