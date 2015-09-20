#lang planet neil/sicp

; Generalizing one- and two-dimensional tables, show how to implement a 
; table in which values are stored under an arbitrary number of keys and 
; different values may be stored under different numbers of keys. The 
; lookup and insert! procedures should take as input a list of keys used to 
; access the table.

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (display-table )
      (display local-table) 
      (newline))
    (define (lookup keys)
      (if (null? keys)
          (error "No keys provided -- LOOKUP")
          (let loop ((record (assoc (car keys) (cdr local-table)))
                     (remaining-keys keys)
                     (local-table (cdr local-table)))
            (cond ((and record (null? (cdr remaining-keys)))
                   (cdr record))
                  (record
                   (loop (assoc (cadr remaining-keys) (cdr record))
                         (cdr remaining-keys)
                         local-table))
                  (else #f)))))
    (define (insert! keys value)
      (begin
        (let loop ((local-table local-table)
                   (keys keys))
          (let* ((key (car keys))
                 (rest (cdr keys))
                 (subtable (if (list? local-table)
                               (assoc key (cdr local-table))
                               #f)))
            (if subtable
                (if (null? rest)
                    (set-cdr! subtable value)
                    (loop subtable rest))
                (cond ((and (null? rest) local-table)
                       (set-cdr! local-table (list (cons key value))))
                      ((null? rest)
                       (cons key value))
                      (local-table
                       (set-cdr! local-table
                                 (cons (list key (loop subtable rest))
                                       (cdr local-table))))
                      (else
                       (cons key (list (loop subtable rest))))))))) 
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display-table) display-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


; Testing

(define t (make-table))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))
(define display-table (t 'display-table))

(put (list 'a 'b 'c) 'hello)
; => 'ok
(put (list 'd 'e) 'hi)
; => 'ok

(display-table)
; => (*table* (d (e . hi)) (a (b (c . hello))))

(get (list 'a 'b 'c))
; => 'hello
(get (list 'd 'e))
; => 'hi
(put (list 'a 'b 'c 'd) 'hi)
; => 'ok
