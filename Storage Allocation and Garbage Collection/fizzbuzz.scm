; Write a program that prints the numbers from 1 to 100. But for multiples of three print
; “Fizz” instead of the number and for the multiples of five print “Buzz”. For numbers
; which are multiples of both three and five print “FizzBuzz”."

(define (integers-from-1-to max)
  (let loop ((n max)
             (acc '()))
    (if (= n 0) acc
        (loop (- n 1)
              (cons n acc)))))

(define (foreach l f)
  (unless (null? l)
    (f (car l))
    (foreach (cdr l) f)))

(define (mod n m)
  (if (< (- n m) 0) n
      (mod (- n m) m)))

(define (multiple-of-3? n)
  (= (mod n 3) 0))

(define (multiple-of-5? n)
  (= (mod n 5) 0))

(define (fizz-buzz n)
  (when (multiple-of-3? n) (display "Fizz"))
  (when (multiple-of-5? n) (display "Buzz"))
  (when (not (or (multiple-of-3? n) (multiple-of-5? n))) (display n))
  (display "\n"))

(foreach (integers-from-1-to 100) fizz-buzz)
