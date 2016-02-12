#lang scheme

;; Use the amb evaluator to solve the following puzzle:

    ;; Mary Ann Moore's father has a yacht and so has each of his four friends: Colonel 
    ;; Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker. Each of the five also has one 
    ;; daughter and each has named his yacht after a daughter of one of the others. Sir 
    ;; Barnacle's yacht is the Gabrielle, Mr. Moore owns the Lorna; Mr. Hall the Rosalind. 
    ;; The Melissa, owned by Colonel Downing, is named after Sir Barnacle's daughter. 
    ;; Gabrielle's father owns the yacht that is named after Dr. Parker's daughter. 
    ;; Who is Lorna's father? 

;; Try to write the program so that it runs efficiently (see exercise 4.40). Also determine 
;; how many solutions there are if we are not told that Mary Ann's last name is Moore. 

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

(define father car)
(define daughter cadr)
(define yacht caddr)

(define (different-daughters? father-daughter-yachts)
  (= (length 
       (filter (lambda (fdy) (eq? (father fdy) (daughter fdy))) father-daughter-yachts)) 
     0))

(define (father-gabrielle l)
  (if (eq? (daughter (car l)) 'gabrielle)
    (car l)
    (father-gabrielle (cdr l))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (father-daughter-yacht)
  (let ((moore (list 'moore 'mary-ann 'lorna))
        (barnacle (list 'barnacle 'melissa 'gabrielle))
        (downing (list 'downing (amb 'gabrielle 'lorna 'melissa 'mary-ann 'rosalind) 
                       'melissa))
        (hall (list 'hall (amb 'gabrielle 'lorna 'melissa 'mary-ann 'rosalind) 'rosalind))
        (parker (list 'parker (amb 'gabrielle 'lorna 'melissa 'mary-ann 'rosalind)
                      (amb 'gabrielle 'lorna 'melissa 'mary-ann 'rosalind))))

    (let ((father-daughter-yachts (list moore downing hall barnacle parker)))
      (require (different-daughters? father-daughter-yachts))
      (require (distinct? father-daughter-yachts)) 
      (require (distinct? (map yacht father-daughter-yachts))) 
      (require (distinct? (map daughter father-daughter-yachts))) 
      (require (distinct? (map father father-daughter-yachts))) 
      (require (eq? (daughter parker) (yacht (father-gabrielle father-daughter-yachts))))
      (list (list 'moore moore) 
            (list 'downing downing) 
            (list 'hall hall) 
            (list 'barnacle barnacle) 
            (list 'parker parker)))))

(father-daughter-yacht)
; => ((moore (moore mary-ann lorna)) 
;     (downing (downing lorna melissa)) 
;     (hall (hall gabrielle rosalind)) 
;     (barnacle (barnacle melissa gabrielle)) 
;     (parker (parker rosalind mary-ann)))
; So the father of lorna is Mr. Downing.

; For the second question ("What if we do not know Mary-Ann's last name is Moore?") 
; the daughter of moore can be changed for an amb and the code needs to be run again.

