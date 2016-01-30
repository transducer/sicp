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
  (define all-permutations (permutations (list 1 2 3 4 5)))
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
               (not (= (abs (- (smith current-permutation) (fletcher current-permutation))) 1))
               (not (= (abs (- (fletcher current-permutation) (cooper current-permutation))) 1)))
        (list 
          (list 'baker (baker current-permutation)) 
          (list 'cooper (cooper current-permutation)) 
          (list 'fletcher (fletcher current-permutation)) 
          (list 'miller (miller current-permutation)) 
          (list 'smith (smith current-permutation)))
        (try-all (cdr remaining-permutations))))))

; http://stackoverflow.com/questions/20319593/creating-permutation-of-a-list-in-scheme#20320352
(define (permutations s)
  (cond [(empty? s) empty]
        [(empty? (rest s)) (list s)]
        [else
         (let splice [(l '()) (m (first s)) (r (rest s))]
           (append
            (map (lambda (x) (cons m x))
                 (permutations (append l r)))
            (if (empty? r)
                empty
                (splice (cons m l) (car r) (cdr r)))))]))

(multiple-dwelling)
; => ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

