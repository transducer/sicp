(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (div-streams s1 s2) (stream-map / s1 s2))
(define (integers-from n) (stream-cons n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define ones (stream-cons 1 ones))

(define (stream-for-each proc s)
  (if (stream-empty? s)
      'done
      (begin (proc (stream-first s))
             (stream-for-each proc (stream-rest s)))))

(define (display-line x) (newline) (display x))
(define (display-stream s) (stream-for-each display-line s))

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map
              (cons proc (map stream-rest argstreams))))))

(define (scale-stream s factor)
  (stream-map (lambda (e) (* factor e)) s))
