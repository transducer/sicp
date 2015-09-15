#lang planet neil/sicp

;; Instead of representing a queue as a pair of pointers, we can build a 
;; queue as a procedure with local state. The local state will consist of 
;; pointers to the beginning and the end of an ordinary list. Thus, the 
;; make-queue procedure will have the form

; (define (make-queue)
;   (let ((front-ptr ...)
;         (rear-ptr ...))
;     <definitions of internal procedures>
;     (define (dispatch m) ...)
;     dispatch))

;; Complete the definition of make-queue and provide implementations of the 
;; queue operations using this representation. 

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    
    (define (empty-queue?)
      (null? front-ptr))
    
    (define (set-front-ptr! item) 
      (set! front-ptr item)) 
    
    (define (set-rear-ptr! item) 
      (set! rear-ptr item)) 
    
    (define (front-queue)  
      (if (empty-queue?) 
          (error "Queue is empty -- FRONT-QUEUE") 
          (car front-ptr))) 
    
    (define (delete-queue!)
      (if (empty-queue?)
          (error "Queue is empty -- DELETE-QUEUE!")
          (set-front-ptr! (cdr front-ptr))))
    
    (define (insert-queue!) 
      (lambda (item)
        (let ((new-pair (cons item '()))) 
          (cond ((empty-queue?) 
                 (set-front-ptr! new-pair) 
                 (set-rear-ptr! new-pair)) 
                (else  
                 (set-cdr! rear-ptr new-pair) 
                 (set-rear-ptr! new-pair))))) )
    
    (define (print-queue)
      (newline)
      (display front-ptr))
    
    (define (dispatch m)
      (cond ((eq? m 'print-queue) (print-queue))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) (insert-queue!))
            ((eq? m 'delete-queue!) (delete-queue!))))
    
    dispatch))


; Testing

(define q (make-queue))

(q 'print-queue)
; => ()

((q 'insert-queue!) 'a)
(q 'print-queue)
; => (a)

((q 'insert-queue!) 'b)
((q 'insert-queue!) 'c)
(q 'print-queue)
; => (a b c)

(q 'delete-queue!)
(q 'print-queue)
; => (b c)

(q 'front-queue)
; => 'b

(q 'delete-queue!)
(q 'delete-queue!)
(q 'delete-queue!)
; => Queue is empty -- DELETE-QUEUE!
