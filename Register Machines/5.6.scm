;; Ben Bitdiddle observes that the Fibonacci machine's controller sequence has an extra
;; save and an extra restore, which can be removed to make a faster machine. Where are
;; these instructions? 

after-fib-n-1
(restore n)
; (restore continue) unnecessary
(assign n (- (fetch n) 2))
; (save continue) unnecessary
