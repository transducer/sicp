#lang scheme

;; Solve the following ``Liars'' puzzle (from Phillips 1934):

    ;; Five schoolgirls sat for an examination. Their parents -- so they thought -- showed 
    ;; an undue degree of interest in the result. They therefore agreed that, in writing 
    ;; home about the examination, each girl should make one true statement and one untrue 
    ;; one. The following are the relevant passages from their letters:

        ;; Betty: ``Kitty was second in the examination. I was only third.''
        ;; Ethel: ``You'll be glad to hear that I was on top. Joan was second.''
        ;; Joan: ``I was third, and poor old Ethel was bottom.''
        ;; Kitty: ``I came out second. Mary was only fourth.''
        ;; Mary: ``I was fourth. Top place was taken by Betty.'' 

    ;; What in fact was the order in which the five girls were placed? 

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

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (xor a b)
  (and (or a b) 
       (not (and a b))))

(define (liars)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

(liars)
; => ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

