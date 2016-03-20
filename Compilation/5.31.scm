;; In evaluating a procedure application, the explicit-control evaluator always saves and
;; restores the env register around the evaluation of the operator, saves and restores env
;; around the evaluation of each operand (except the final one), saves and restores argl
;; around the evaluation of each operand, and saves and restores proc around the evaluation
;; of the operand sequence. For each of the following combinations, say which of these save
;; and restore operations are superfluous and thus could be eliminated by the compiler's
;; preserving mechanism:

; (f 'x 'y)
; => All redundant

; ((f) 'x 'y)
; => All redundant

; (f (g 'x) y)
; => Save and restore of argl necessary

; (f (g 'x) 'y)
; => Save and restore of argl necessary. Not necessary to save env before evaluation of
;    (g 'x) since 'y is a constant.
