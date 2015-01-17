#lang planet neil/sicp

;; We want to rewrite the procedure cc so that its second 
;; argument is a list of the values of the coins to use rather 
;; than an integer specifying which coins to use. We could then 
;; have lists that defined each kind of currency:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; We could then call cc as follows:

;; (cc 100 us-coins)
;; => 292

;; To do this will require changing the program cc somewhat. It
;; will still have the same form, but it will access its second
;; argument differently, as follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; Define the procedures first-denomination, 
;; except-first-denomination, and no-more? in terms of primitive
;; operations on list structures. 

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;; Does the order of the list
;; coin-values affect the answer produced by cc? Why or why not?

(cc 100 us-coins)            ;; => 292

(cc 100 (list 25 50 10 5 1)) ;; => 292

;; Order does not matter.

;; Quoting SICP (1.2.2 Tree Recursion):
;; "[O]bserve that the ways to make change can be divided into
;;  two groups: those that do not use any of the first kind of
;;  coin, and those that do. Therefore, the total number of ways
;;  to make change for some amount is equal to the number of ways
;;  to make change for the amount without using any of the first
;;  kind of coin, plus the number of ways to make changeassuming
;;  that we do use the first kind of coin. But the latter number
;;  is equal to the number of ways to make change for the amount
;;  that remains after using a coin of the first kind."

;; This does also work when you start with a lower kind of coin.