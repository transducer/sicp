#lang racket

(require racket/include)
(include (file "../Streams, Part 1/racket-stream-extra.rkt"))

;; Redo exercise 3.5 on Monte Carlo integration in terms of streams. The 
;; stream version of estimate-integral will not have an argument telling 
;; how many trials to perform. Instead, it will produce a stream of 
;; estimates based on successively more trials.

; Played around with answer at http://community.schemewiki.org/?sicp-ex-3.82

(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (* (random) range))))

(define (random-number-pairs low1 high1 low2 high2) 
  (stream-cons (cons (random-in-range low1 high1) 
                     (random-in-range low2 high2)) 
               (random-number-pairs low1 high1 low2 high2))) 

(define (monte-carlo experiment-stream passed failed) 
  (define (next passed failed) 
    (stream-cons (/ passed (+ passed failed)) 
                 (monte-carlo (stream-rest experiment-stream) 
                              passed 
                              failed))) 
  (if (stream-first experiment-stream) 
      (next (+ passed 1) failed) 
      (next passed (+ failed 1)))) 

(define (estimate-integral p x1 x2 y1 y2) 
  (let ((area (* (- x2 x1) (- y2 y1))) 
        (randoms (random-number-pairs x1 x2 y1 y2))) 
    (scale-stream (monte-carlo (stream-map p randoms) 0 0) area))) 

;; Testing: get the value of pi 
(define (sum-of-squares x y) (+ (* x x) (* y y))) 
(define f 
  (lambda (x) (not (> (sum-of-squares (- (car x) 1) (- (cdr x) 1)) 1)))) 
(define pi-stream (estimate-integral f -1 1 -1 1)) 

(display-stream pi-stream)
; => Nearing pi
