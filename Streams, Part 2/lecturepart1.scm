#lang scheme

; Streams... Sort of signal-processing way of putting systems together.

; The key idea: Decoupling the apparent order of event in our programs from
;               the actual order of events in the computer.

(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (n-thstream (-1+ n) (tail s))))

; Those elements are not going to get computed until we force them.

(define (print-stream s )
  (cond ((empty-stream? s) 'done)
        (else (print (head s))
              (print-stream (tail s)))))

(define (integers-from n)
  (cons-stream
   n
   (integers-from (+ n 1))))

(define integers 
  (integers-from 1))

(nth-stream 20 integers)
; => 21

(define (no-seven x)
  (not (= (remainder x 7) 
          0)))

(define ns (filter no-seven 
                   integers))

(nth-stream 100 ms)
; => 117

(print-stream ns)
; => 0, 1, 2, 3, 4, 5, 6, 8

; Is it really true that the data structure integers is all the integers?

; That's sort of a philosophical question: if something is there when you
; look is it really something that is there? :)

; Same sense in which the money in your savings account is in the bank.

; We started the course with Heron's of Alexandria's formula to compute
; the square root.

; Here's Erathosthenes sieve: a method for computing all of the primes.

; Look at all the integers, start from the first prime you find and cross
; out all multiples. Look what you have left and cross out all the things
; divisible by that. What will remain is a list of all the primes.

(define (sieve s)
  (cons-stream
   (head s)
   (sieve (filter
           (lambda (x)
             (not
              (divisible? x (head s))))
           (tail s)))))

(define primes
  (sieve (integers-from 2)))

(nth-stream 20 primes)
; => 73

(print-stream primes)
; => [prints primes]

; Mix signal processing view of the world with things like recursion.
