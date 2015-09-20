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
      (define (foldl op acc lst)
        (if (null? lst)
            acc
            (foldl op (op acc (car lst)) (cdr lst))))
      (define (descend table key)
        (let ((record (assoc key (cdr table))))
          (if record
              record
              (let ((new (cons (list key) (cdr table))))
                (set-cdr! table new)
                (car new)))))
      (set-cdr! (foldl descend local-table keys) value))
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
(put (list 'd 'e) 'hi)

(display-table)
; => (*table* (d (e . hello)) (a (b (c . hello))))

(get (list 'a 'b 'c))
; => 'hello
(get (list 'd 'e))
; => 'hi
(put (list 'a 'b 'c 'd) 'hi)
; => ERROR mcar: contract violation 
;    Seems like unwanted behaviour. Preferably it would be possible to add
;    symbols to existing parts of a table. But for now I see no way on how
;    to implement that.







