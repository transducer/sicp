#lang scheme

;; With the grammar given above, the following sentence can be parsed in five different 
;; ways: ``The professor lectures to the student in the class with the cat.'' Give the 
;; five parses and explain the differences in shades of meaning among them. 

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

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(parse '(the professor lectures to the student in the class with the cat))
; => amb tree exhausted... Me too. I suspect the above amb as defined in the macro does not 
; undo assignments. I might try later once the evaluator is introduced. TODO!

