#lang racket

;; How many additions are performed when we compute the nth Fibonacci 
;; number using the definition of fibs based on the add-streams procedure? 
;; Show that the number of additions would be exponentially greater if we 
;; had implemented (delay <exp>) simply as (lambda () <exp>), without using 
;; the optimization provided by the memo-proc procedure described in 
;; section 3.5.1.

With memoization:
n-1 (each previous value is added once)

Without memoization:
fib(n) takes fib(n-1) + fib(n-2) times 
(each previous value is recalculated from all previous values of that 
      value and added)
