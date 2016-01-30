#lang scheme

;; Does the order of the restrictions in the multiple-dwelling procedure affect the answer? 
;; Does it affect the time to find an answer? If you think it matters, demonstrate a faster 
;; program obtained from the given one by reordering the restrictions. If you think it does 
;; not matter, argue your case. 

; => Yes. If the restrictions are ordered in a way so that the search space decreases
;    more (and the restriction is quicker to calculate then the search space reduction) 
;    the answer will be found faster.

; Example:

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require                                                 ; <-expensive calculation to end
      (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

