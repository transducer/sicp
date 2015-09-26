#lang scheme

;; Without running the program, describe the elements of the stream defined
;; by

(define s (cons-stream 1 (add-streams s s)))

; It will contain a stream where each successive values doubles the
; previous value (the power of two of the index, starting from 1).
; e.g.,

1, 2, 4, 8, 16 ...