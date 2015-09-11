#lang scheme

;; Consider the bank account objects created by make-account, with the password modification 
;; described in exercise 3.3. Suppose that our banking system requires the ability to make 
;; joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should 
;; take three arguments. The first is a password-protected account. The second argument must 
;; match the password with which the account was defined in order for the make-joint 
;; operation to proceed. The third argument is a new password. Make-joint is to create an 
;; additional access to the original account using the new password. For example, if 
;; peter-acc is a bank account with password open-sesame, then

; (define paul-acc
;   (make-joint peter-acc 'open-sesame 'rosebud))

;; will allow one to make transactions on peter-acc using the name paul-acc and the password 
;; rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new 
;; feature. 

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch pw m)
    (cond ((not (eq? password pw)) (lambda (m) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint account original-password new-password)
  (define bad-password 
    (lambda (m) "Incorrect password"))
  (define (correct-password? account password)
    (not (eq? ((account password 'withdraw) 0) bad-password))) ; Stringly typed
  (if (correct-password? account original-password)
      (lambda (pw m)
        (if (eq? pw new-password)
            (account original-password m)
            bad-password))
      bad-password))


; Testing

(define peter-acc (make-account 10 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 5)
; => 5

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 5)
; => 0
((paul-acc 'open-sesame 'withdraw) 5)
; => "Incorrect password"
((paul-acc 'other-password 'withdraw) 5)
; => "Incorrect password"

(define edwin-acc
  (make-joint peter-acc 'wrong-password 'rosebud))

((edwin-acc 'rosebud 'withdraw) 5)
; => "Incorrect password" (note: not clear that this is because joining accounts failed)
