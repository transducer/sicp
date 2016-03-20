;; Using the preserving mechanism, the compiler will avoid saving and restoring env around
;; the evaluation of the operator of a combination in the case where the operator is a symbol
;; . We could also build such optimizations into the evaluator. Indeed, the explicit-control
;; evaluator of section 5.4 already performs a similar optimization, by treating combinations
;; with no operands as a special case.

;; a. Extend the explicit-control evaluator to recognize as a separate class of expressions
;; combinations whose operator is a symbol, and to take advantage of this fact in evaluating
;; such expressions.

ev-application
  (save continue)
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (test (op symbol?) (reg exp))                             ; test for symbol
  (branch (label ev-appl-operator-symbol))
  (save env)
  (save unev)      
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-operator-symbol
  (assign continue (label ev-appl-did-operator-no-restore))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env)
ev-appl-did-operator-no-restore
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)

;; b. Alyssa P. Hacker suggests that by extending the evaluator to recognize more and more
;; special cases we could incorporate all the compiler's optimizations, and that this would
;; eliminate the advantage of compilation altogether. What do you think of this idea? 

; => Nope. Checking every time is slow. In the compiler we have this problem only during
;    compilation once.
