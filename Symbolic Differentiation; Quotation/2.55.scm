#lang scheme

;; Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

;; To her surprise, the interpreter prints back quote. Explain. 

;; => The car is similar to quote, so (car ''abracadabra)s can be rewritten
;;    as:

(car (quote (quote abracadabra)))

;; The car of the list (quote (quote abracadabra)) is quote.