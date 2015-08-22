#lang scheme

; Now will follow some examples.

; They will probably be the worst way to compute some
; of the things we will compute. But we'll get a feel
; for how a program can be a rule to evolve a process.

; Fibonacci

; 0 1 1 2 3 5 8 13 21 34 55 ...
; 0 1 2 3 4 5 6 7  8  9  10 ... 

; "Grows very fast. Just like rabbits. Why so fast? I am
;  not going to hazard a guess." - GJS

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 3) ; => 2
(fib 4) ; => 3
(fib 5) ; => 5
(fib 6) ; => 8

;          fib4
;       fib3  fib 2
;   fib2 fib1 fib1 fib0
;   fib1 fib0  1    0
;     1   0

; We can see this is an extremely expensive way to 
; calculate fibonacci number. We are computing everything
; twice. 
; This is exponential in time. Time = O(fib(n))
; And space is O(n). As big as the number.

; Towers of Hanoi. Think about it recursively.

; (define (move n from to spare)
;   (cond ((= n 0) "done")
;     (else
;       (move (-1+ n) from spare to)
;       (print-move from to)
;       (move (-1+ n) spare to from))

; "I want you to practice." -GJS
