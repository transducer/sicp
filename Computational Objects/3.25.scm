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
    (define (insert! key-list value table)
      (let loop ((remaining-key-list key-list)
                 (key (car key-list))
                 (record (assoc (car key-list) (cdr table)))
                 (subtable table))
        (if record
            (if (pair? record)
                (loop (cdr remaining-key-list)
                      (car remaining-key-list)
                      (assoc (cadr remaining-key-list) (cddr subtable))
                      (cdr subtable))
                (set-cdr! record value))
            (set-cdr! subtable
                      (cons (cons key value) 
                            (cdr subtable)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display-table) display-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (add-keys-and-value keys value)
  (let loop ((keys keys)
             (result (list '*table*)))
    (if (not (pair? keys)) 
        (cons result value)
        (loop (cdr keys) (list (car keys) result)))))

(define test (add-keys-and-value (list 'a 'b) 'hello))

; (*table* (a (b . hello)))
(display (list '*table* (list 'a (cons 'b 'hello))))
(newline)
(display test)
