;; Define a rule that says that person 1 can replace person 2 if either person 1 does the 
;; same job as person 2 or someone who does person 1's job can also do person 2's job, and 
;; if person 1 and person 2 are not the same person. Using your rule, give queries that 
;; find the following:

;; a.  all people who can replace Cy D. Fect;

(rule (can-replace ?staff-person ?replacement)
      (and (or (same (job ?replacement) (job ?staff-person))
               (can-do-job ?replacement (job ?staff-person)))
           (not (same ?staff-person ?replacement))))


(can-replace ?staff-person (Fect Cy D))

;; b.  all people who can replace someone who is being paid more than they are, together 
;; with the two salaries.

(and (can-replace ?staff-person ?replacement)
     (salary ?staff-person ?staff-person-salary)
     (salary ?replacement ?replacement-salary)
     (lisp-value > ?staff-person-salary ?replacement-salary))

