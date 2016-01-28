#lang planet neil/sicp

;; Design and carry out some experiments to compare the speed of the original metacircular 
;; evaluator with the version in this section. Use your results to estimate the fraction 
;; of time that is spent in analysis versus execution for various procedures.

; Full metacircular evaluator can be found here:
; https://mitpress.mit.edu/sicp/code/ch4.scm

; Full metacircular evaluator with analysis can be found here:
; https://mitpress.mit.edu/sicp/code/ch4-analyzingmceval.scm

; Note that the apply function needs to be renamed, to for example metacircular-apply, to
; be able to run it in DrRacket.

; About half of the time seems to be spent analysing.
