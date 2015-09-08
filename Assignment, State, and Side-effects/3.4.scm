#lang scheme

;; Modify the make-account procedure of exercise 3.3 by adding another local state variable so 
;; that, if an account is accessed more than seven consecutive times with an incorrect 
;; password, it invokes the procedure call-the-cops. 

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define attempts 0)
  (define (call-the-cops) "Calling the cops")
  (define (dispatch pw m)
    (cond ((not (eq? password pw))
           (begin
             (set! attempts (+ attempts 1))
             (if (> attempts 7)
                 (lambda (m) (call-the-cops))
                 (lambda (m) "Incorrect password"))))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)


; Testing

(define acc (make-account 100 'secret-password))

((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
((acc 'some-other-password 'deposit) 50)
; => "Calling the cops"