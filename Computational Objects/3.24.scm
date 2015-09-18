#lang planet neil/sicp

; In the table implementations above, the keys are tested for equality 
; using equal? (called by assoc). This is not always the appropriate test. 
; For instance, we might have a table with numeric keys in which we don't 
; need an exact match to the number we're looking up, but only a number 
; within some tolerance of it. Design a table constructor make-table that 
; takes as an argument a same-key? procedure that will be used to test 
; ``equality'' of keys. Make-table should return a dispatch procedure that 
; can be used to access appropriate lookup and insert! procedures for a 
; local table. 

(define (make-table same-key?-proc)
  (let ((local-table (list '*table*))
        (equal? same-key?-proc))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


; Testing

(define (same-key? a b)
  (let ((margin 0.5))
    (if (> a b)
        (and (< (- margin b) a)
             (> (+ margin b) a))
        (and (< (- margin a) b)
             (> (+ margin a) b)))))

(define t (make-table same-key?))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))

(put 1 1 'hello)
; => 'ok

(get 1 1)
; => 'hello

(get 2 2)
; => #f

(get 0.51 1.49)
; => 'hello

(get 0.49 1.49)
; => #f

(get 0.5 1.5)
; => #f
