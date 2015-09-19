#lang planet neil/sicp

; Generalizing one- and two-dimensional tables, show how to implement a 
; table in which values are stored under an arbitrary number of keys and 
; different values may be stored under different numbers of keys. The 
; lookup and insert! procedures should take as input a list of keys used to 
; access the table.

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (display-table )
      (display local-table))
    (define (foldr op initial lst)
      (if (null? lst)
          initial
          (op (car lst) (foldr op initial (cdr lst)))))
    (define (add-keys-and-value keys value)
      (cond ((null? keys)
             (error "No key provided - ADD-KEYS-AND-VALUE"))
            ((pair? keys)
             (foldr list value keys))
            (else (cons keys value))))
    (define (lookup key-list)
      (let loop ((remaining-key-list key-list)
                 (subtable (assoc (car key-list) (cdr local-table)))
                 (local-table (cdr local-table)))
        (if subtable
            (let ((record (assoc (cadr remaining-key-list) (cdr subtable))))
              (if record
                  (loop (cdr remaining-key-list)
                        (assoc (caddr remaining-key-list) 
                               (cdr local-table)) 
                        (cdr local-table))
                  (cdr subtable))
              false))))
    (define (insert! key-list value)
      (let loop ((remaining-key-list key-list)
                 (key (car key-list))
                 (record (assoc (car key-list) (cdr local-table)))
                 (subtable local-table))
        (if record
            (if (pair? record)
                (loop (cdr remaining-key-list)
                      (car remaining-key-list)
                      (assoc (cadr remaining-key-list) (cddr subtable))
                      (cdr subtable))
                (set-cdr! record value))
            (set-cdr! subtable
                      (add-keys-and-value remaining-key-list value))))
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

(display-table)
; => (*table* a (b (c hello)))... Should be (*table* (a (b (c hello))))!

(get (list 'a 'b))






