#lang scheme

;; The ``eight-queens puzzle'' asks how to place eight queens on a chessboard
;; so that no queen is in check from any other (i.e., no two queens are in 
;; the same row, column, or diagonal). One possible solution is shown in 
;; figure 2.8. One way to solve the puzzle is to work across the board, 
;; placing a queen in each column. Once we have placed k - 1 queens, we must
;; place the kth queen in a position where it does not check any of the
;; queens already on the board. 
;; 
;; We can formulate this approach recursively: 
;; Assume that we have already generated the sequence of all possible ways to
;; place k - 1 queens in the first k - 1 columns of the board. For each of 
;; these ways, generate an extended set of positions by placing a queen in 
;; each row of the kth column. Now filter these, keeping only the positions 
;; for which the queen in the kth column is safe with respect to the other
;; queens. This produces the sequence of all ways to place k queens in the 
;; first k columns. By continuing this process, we will produce not only one
;; solution, but all solutions to the puzzle.
;;
;; We implement this solution as a procedure queens, which returns a 
;; sequence of all solutions to the problem of placing n queens on an n× n 
;; chessboard. Queens has an internal procedure queen-cols that returns the
;; sequence of all ways to place queens in the first k columns of the board.
;; 
;; (define (queens board-size)
;;   (define (queen-cols k)  
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions) (safe? k positions))
;;          (flatmap
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-position new-row k rest-of-queens))
;;                  (enumerate-interval 1 board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))
;; 
;; In this procedure rest-of-queens is a way to place k - 1 queens in the 
;; first k - 1 columns, and new-row is a proposed row in which to place the 
;; queen for the kth column. Complete the program by implementing the 
;; representation for sets of board positions, including the procedure 
;; adjoin-position, which adjoins a new row-column position to a set of 
;; positions, and empty-board, which represents an empty set of positions. 
;; You must also write the procedure safe?, which determines for a set of 
;; positions, whether the queen in the kth column is safe with respect to 
;; the others. (Note that we need only check whether the new queen is safe -- 
;; the other queens are already guaranteed safe with respect to each other.) 

;; Import:
(define (filter predicate sequence) 
  (cond ((null? sequence) nil) 
        ((predicate (car sequence)) 
         (cons (car sequence)  
               (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence))))) 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define nil '())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ low 1) high))))

;; Start answering the question:
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Define empty board has 64 squares with index and 0 or 1, where
;; 1 means there is a queen.
;; (define empty-board 
;;   (map (lambda (i) (cons i (cons 0 nil))) (enumerate-interval 1 64)))

;; Forget that.

;; Bill the Lizard to the rescue
;; Source: 
;; http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html

;; Since a position is just a row-column pair, we can use what should by now
;; be the familiar method for representing pairs.

(define (make-position row col)
  (cons row col))

(define (position-row position)
  (car position))

(define (position-col position)
  (cdr position))

;; We can represent an empty board with null, and adjoin a new position to a
;; board using list operations.

(define empty-board nil)

(define (adjoin-position row col positions)
  (append positions (list (make-position row col))))

;; A queen is safe if no other queen is in the same row, column, or diagonal.
;; Since we're only placing one queen in each column at a time, we can skip 
;; checking the column.

(define (safe? col positions)
  (let ((kth-queen (list-ref positions (- col 1)))
        (other-queens (filter (lambda (q)
                                (not (= col (position-col q))))
                              positions)))
    (define (attacks? q1 q2)
      (or (= (position-row q1) (position-row q2))
          (= (abs (- (position-row q1) (position-row q2)))
             (abs (- (position-col q1) (position-col q2))))))
    
    (define (iter q board)
      (or (null? board)
          (and (not (attacks? q (car board)))
               (iter q (cdr board)))))
    (iter kth-queen other-queens)))

;; The safe? procedure starts be defining the symbols kth-queen and 
;; other-queens. We use the list-ref procedure introduced in SICP section 
;; 2.2.1 to separate the kth queen from the rest of the board. Next we define
;; an attacks? procedure that takes two queens and returns true if they are
;; in the same row or on the same diagonal. Then we define an iterative 
;; helper procedure to check and see if the kth queen attacks any of the 
;; others.

;; Testing (match http://oeis.org/A000170)

(length (queens 5))
;; => 10
(length (queens 6))
;; => 4
(length (queens 7))
;; => 40
(length (queens 8))
;; => 92
(length (queens 9))
;; => 352
(length (queens 10))
;; => 724