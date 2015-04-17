#lang scheme

;; Suppose we represent vectors v = (v_i) as sequences of numbers, and matrices m = (m_ij) as
;; sequences of vectors (the rows of the matrix). For example, the matrix

;;  ---------
;; | 1 2 3 4 |
;; | 4 5 6 6 |
;; | 6 7 8 9 |
;;  ---------

;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation,
;; we can use sequence operations to concisely express the basic matrix and vector operations.
;; These operations (which are described in any book on matrix algebra) are the following:

;; (dot-product v w)     returns the sum SUM_i v_i w_i
;; (matrix-*-vector m v) returns the vector t, where t_i  = SUM_j m_ij v_j
;; (matrix-*-matrix m n) returns the matrix p, where p_ij = SUM_k m_ik n_kj
;; (transpose m)         returns the matrix n, where n_ij = m_ij

;; We can define the dot product as

;; (define (dot-product v w)
;;   (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for computing the other matrix
;; operations. (The procedure accumulate-n is defined in exercise 2.36.)

;; (define (matrix-*-vector m v)
;;   (map <??> m))
;; (define (transpose mat)
;;   (accumulate-n <??> <??> mat))
;; (define (matrix-*-matrix m n)
;;   (let ((cols (transpose n)))
;;     (map <??> m)))

;; accumulate-n from 2.36:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define nil '())

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Answer the question:
(define (matrix-*-vector m v)
  (map (lambda (x) (* v x)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector n-cols m-row))
         m))) 
