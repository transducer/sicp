#lang scheme

;; Implement a new special form ramb that is like amb except that it searches alternatives 
;; in a random order, rather than from left to right. Show how this can help with Alyssa's 
;; problem in exercise 4.49. 

; Advice from: http://eli.thegreenplace.net/2008/01/11/sicp-section-433

(define (analyze-ramb exp)
  (analyze-amb 
    (cons 'ramb 
          (shuffle-begin-position (amb-choices exp)))))

(define (shuffle-begin-position l)
  ; set start position random
  (let ((new-start-index (random (length l))))
    (append (drop l new-start-index) (take l new-start-index))))

; If there is a random start, generation of sentences by an "inverse parser" (generator) 
; will not lead to recursion.
