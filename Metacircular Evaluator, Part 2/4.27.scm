;; Suppose we type in the following definitions to the lazy evaluator:

; (define count 0)
; (define (id x)
;   (set! count (+ count 1))
;   x)

;; Give the missing values in the following sequence of interactions, and explain your 
;; answers.

; (define w (id (id 10)))
;;; L-Eval input:
; count
;;; L-Eval value:
1
; => called once in definition of w

;;; L-Eval input:
; w
;;; L-Eval value:
10
; => Just returns the identity

;;; L-Eval input:
; count
;;; L-Eval value:
2
; => The other id in w is also evaluated and increments count

