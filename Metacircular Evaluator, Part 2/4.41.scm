#lang scheme

;; Write an ordinary Scheme program to solve the multiple dwelling puzzle. 

; Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment 
; house that contains only five floors. Baker does not live on the top floor. Cooper does 
; not live on the bottom floor. Fletcher does not live on either the top or the bottom 
; floor. Miller lives on a higher floor than does Cooper. Smith does not live on a floor 
; adjacent to Fletcher's. Fletcher does not live on a floor adjacent to Cooper's. 
; Where does everyone live? 

(define (multiple-dwelling)
  ; Do it the ugly brute force way by trying all possible combinations
  ; Data structure is such that first position is baker, second cooper, third fletcher,
  ; fourth miller, fifth smith
  (define all-permutations (permutations 5 (list 1 2 3 4 5)))
  (define baker car)
  (define cooper cadr)
  (define fletcher caddr)
  (define miller cadddr)
  (define smith (lambda (p) (car (cddddr p)))) ; caddddr
  (let try-all ((remaining-permutations all-permutations))
    (let ((current-permutation (car remaining-permutations)))
      (if (and (not (= (baker current-permutation) 5))
               (not (= (cooper current-permutation) 1))
               (not (= (fletcher current-permutation) 1))
               (not (= (fletcher current-permutation) 5))
               (> (miller current-permutation) (cooper current-permutation))
               (= (abs (- (smith current-permutation) (fletcher current-permutation))) 1)
               (= (abs (- (fletcher current-permutation) (cooper current-permutation))) 1))
        (list 
          (list 'baker (baker current-permutation)) 
          (list 'cooper (cooper current-permutation)) 
          (list 'fletcher (fletcher current-permutation)) 
          (list 'miller (miller current-permutation)) 
          (list 'smith (smith current-permutation)))
        (try-all (cdr remaining-permutations))))))

; stackoverflow.com/questions/3179931/how-do-i-generate-all-permutations-of-certain-size
; -with-repetitions-in-scheme#3181532

(define (permutations size elements)
  (if (zero? size)
    '(())
    (flatmap (lambda (p)            ; For each permutation we already have:
               (map (lambda (e)     ;   For each element in the set:
                      (cons e p))   ;     Add the element to the perm'n.
                    elements))
             (permutations (- size 1) elements))))

(define (flatmap f lst)
  (apply append (map f lst)))

(multiple-dwelling)
; => ((baker 1) (cooper 3) (fletcher 2) (miller 4) (smith 1))

