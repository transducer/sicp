#lang scheme

;; Many languages support a variety of iteration constructs, such as do, for, while, and 
;; until. In Scheme, iterative processes can be expressed in terms of ordinary procedure 
;; calls, so special iteration constructs provide no essential gain in computational 
;; power. On the other hand, such constructs are often convenient. Design some iteration 
;; constructs, give examples of their use, and show how to implement them as derived 
;; expressions. 

; I'll implement for as a derived expression

(define (for->proc initializer stop-condition incrementer expr)
  '(let for ((i initializer))
    (if (stop-condition) 'done
      (begin (expr)
             (for (incrementer i))))))
           
; The beginning of an idea. Would like to see a correct implementation.

