#lang racket

;; Write a procedure stream-limit that takes as arguments a stream and a 
;; number (the tolerance). It should examine the stream until it finds two 
;; successive elements that differ in absolute value by less than the 
;; tolerance, and return the second of the two elements. Using this, we 
;; could compute square roots up to a given tolerance by

; (define (sqrt x tolerance)
;   (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (define (stream-cadr s) 
    (stream-first (stream-rest s)))
  (if (< (abs (- (stream-cadr s) (stream-first s))) 
         tolerance)
      (stream-cadr s)
      (stream-limit (stream-rest s) tolerance)))


; Testing

(define s 
  (stream-cons 1 
               (stream-cons 2 
                            (stream-cons 2.4 
                                         (stream-cons 2.3 '())))))

(stream-limit s 0.5)
; => 2.4
