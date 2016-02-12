#lang scheme

;; Exercise 2.42 described the ``eight-queens puzzle'' of placing queens on a chessboard 
;; so that no two attack each other. Write a nondeterministic program to solve this puzzle. 

; Introduce amb in our language:
; Source: http://community.schemewiki.org/?amb

;;; FAIL is called to backtrack when a condition fails.  At the top 
;;; level, however, there is no more to backtrack, so we signal an 
;;; error with SRFI 23. 

(define fail 
  (lambda () 
    (error "amb tree exhausted"))) 

(define-syntax amb 
  (syntax-rules () 
                ((amb) (fail))                                ; two shortcuts. 
                ((amb expression) expression) 

                ((amb expression ...) 
                 (let ((fail-save fail)) 
                   ((call-with-current-continuation           ; capture a continuation to 
                      (lambda (k-success)                     ;   which we return possibles. 
                        (call-with-current-continuation 
                          (lambda (k-failure)                 ; k-failure will try the next 
                            (set! fail                        ;   possible expression. 
                              (lambda () (k-failure #f))) 
                            (k-success                        ; note that the expression is 
                              (lambda ()                       ;   evaluated in tail position 
                                expression))))                 ;   with respect to amb. 
                        ... 
                        (set! fail fail-save)                 ; finally, if this is reached, 
                        fail-save)))))))                      ;   we restore the saved fail. 

(define (require condition) 
  (if (not condition) 
    (fail) '())) 

; Answer question:

(define (square) (amb 'empty 'queen))

(define (create-board)
  (let loop ((acc (list square)))
    (if (= (length acc) 64)
      acc
      (loop (cons square acc))))) 

(define (queen-count section)
  (length (filter (lambda (e) (eq? e 'queen)) section)))

(define (max-one-queen-per-row board)
  (let loop ((board board))
    (if (null? board)
      true
      (let ((row (take board 8)))
        (if (<= (queen-count row) 1)
          (loop (drop board 8))
          false)))))

(define (max-one-queen-per-column board))
;; TODO

(define (max-one-queen-per-diagonal board))
;; TODO

(define (print-board board)
  (let loop ((board board))
    (if (= (mod (length board) 8) 0)
      (begin (new-line)
             (display (car board))
             (loop (cdr board)))
      (begin 
        (display (car board))
        (loop (cdr board))))))

(define (eight-queens)
  (let ((board (create-board)))
    (require (= (queen-count board) 8))
    (require (max-one-queen-per-row board))
    (require (max-one-queen-per-column board))
    (require (max-one-queen-per-diagonal board))
    (print-board board)))

