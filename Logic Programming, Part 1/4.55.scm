;; Give simple queries that retrieve the following information from the data base:

;; a. all people supervised by Ben Bitdiddle;

(supervisor ?x (Bitdiddle Ben))

;; b. the names and jobs of all people in the accounting division;

(job ?x (accounting . ?y))

;; c. the names and addresses of all people who live in Slumerville.

(address ?x (Slumerville . ?y))

