;; Define a rule that says that a person is a ``big shot'' in a division if the person 
;; works in the division but does not have a supervisor who works in the division. 

; From lecture:

(rule
  (bigshot ?x ?dept)
  (and
    (job ?x (?dept . ?y))
    (not (and (supervisor ?x ?z)
              (job ?z (?dept . ?w))))))

