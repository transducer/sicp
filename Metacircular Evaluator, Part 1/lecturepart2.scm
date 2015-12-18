; A little bit of practice, understanding what it is we just showed you.
; We are formally stepping through an evaluation.

; Source: https://github.com/f03lipe/sicp-code/blob/master/code/7A.scm

;# BOARD 0:37:30
(eval 	'(((λ(x)(λ(y)(+ x y))) 3) 4)		<e0>)

(apply 	(eval '((λ(x)  λ(y) (+ x y))) 3)	<e0>)
		(evlist '(4) <e0>))

(apply 	(eval '((λ(x) (λ(y) (+ x y))) 3)	<e0>)
		(cons 	(eval 	'4 	<e0>)
				(evlist '() <e0>)))

(apply 	(eval '((λ(x) (λ(y) (+ x y))) 3)	<e0>)
		(cons 4 '()))

(apply 	(eval '((λ(x) (λ(y)(+ x y))) 3)		<e0>)
		'(4))
;# END BOARD



;# BOARD 0:45:00
(apply 	(apply 	(eval '(λ(x) (λ(y) (+ x y))) 	<e0>)
				'(3))
		'(4))

(apply 	(apply 	'(CLOSURE ((x)(λ(y) (+ x y)))	<e0>)
				'(3))
		'(4))

(apply 	(eval 	'(λ(y) (+ x y)) <e1>)
		'(4))
;# END BOARD



;# BOARD 0:48:35
(apply 	'(CLOSURE ((y) (+ x y)) <e1>)
		'(4))

(eval 	'(+ x y) <e2>)

(apply 	(eval '+ 		<e2>)
		(evlist '(x y) 	<e2>))

(apply 	'+	'(3 4))

7
; # END BOARD