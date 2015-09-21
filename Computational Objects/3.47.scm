#lang scheme

; A semaphore (of size n) is a generalization of a mutex. Like a mutex, a 
; semaphore supports acquire and release operations, but it is more general 
; in that up to n processes can acquire it concurrently. Additional 
; processes that attempt to acquire the semaphore must wait for release 
; operations. Give implementations of semaphores

; a. in terms of mutexes

(define (make-semaphore n)
  (let ((the-mutex (make-mutex))
        (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (the-mutex 'acquire)
             (if (= count n)
                 (begin
                   (the-mutex 'release)
                   (the-semaphore 'acquire))
                 (begin
                   (set! count (+ count 1))
                   (the-mutex 'release))))
            ((eq? m 'release)
             (the-mutex 'acquire)
             (if (> count 0)
                 (set! count (- count 1)))
             (the-mutex 'release))))
    the-semaphore))

; b. in terms of atomic test-and-set! operations. 

(define (make-semaphore n) 
  (let ((access-mutex (list false)) 
        (clients 0)) 
    (define (the-semaphore message) 
      (cond ((eq? message 'acquire) 
             (if (test-and-set! access-mutex) 
                 (the-semaphore 'acquire)) 
             (cond ((> clients n) 
                    (clear! access-mutex) 
                    (the-semaphore 'acquire)) 
                   (else 
                    (set! clients (+ clients 1)) 
                    (clear! access-mutex)))) 
            ((eq? message 'release) 
             (if (test-and-set! access-mutex) 
                 (the-semaphore 'release)) 
             (set! clients (- clients 1)) 
             (clear! access-mutex)))) 
    the-semaphore)) 

; Solutions obtained from
; a. https://wqzhang.wordpress.com/2009/08/04/sicp-exercise-3-47/
; b. http://community.schemewiki.org/?sicp-ex-3.47
