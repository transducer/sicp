#lang racket

;; Define a procedure mul-streams, analogous to add-streams, that produces 
;; the elementwise product of its two input streams. Use this together with 
;; the stream of integers to complete the following definition of the 
;; stream whose nth element (counting from 0) is n + 1 factorial:

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (stream-for-each proc s)
  (if (stream-empty? s)
      'done
      (begin (proc (stream-first s))
             (stream-for-each proc (stream-rest s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integers-from n)
  (stream-cons n
               (integers-from (+ n 1))))

(define integers 
  (integers-from 1))

(define factorials 
  (stream-cons 1 
               (mul-streams 
                factorials
                integers)))


; Testing

(define s1 (stream-cons 1 (stream-cons 2 '())))
(define s2 (stream-cons 1 (stream-cons 2 '())))

(display-stream (mul-streams s1 s2))
; => 1, 4, 'done

(display-stream integers)
; => 1, 2, 3, 4, ...

(display-stream factorials)
; => 1
;    1
;    2
;    6
;    24
;    120
;    720
;    5040
;    40320
;    362880
;    3628800
;    39916800
;    479001600
;    6227020800
;    87178291200
;    1307674368000
;    20922789888000
;    355687428096000
