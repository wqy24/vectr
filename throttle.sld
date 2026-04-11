(define-library (vectr throttle)
 (import (scheme base) (scheme inexact) (srfi 1) (vectr head))
 (export throttler)
 (begin
  (define (enveloped e1 e2 y)
   (+ (* (/ (- e1 e2)) (+ y 1)) e2))
  (define (throttler obj head) #;(node1 node2 freq offset envelope1 envelope)
   (lambda (x)
    (let [[n1 (node1 obj)]
          [n2 (node2 obj)]
          [f  (freq  obj)]
          [e1 (envelope1 obj)]
          [e2 (envelope2 obj)]
	  [o  (offset obj)]]
     (enveloped (e1 x) (e2 x) (enveloped n1 n2 (cos (* 2 (acos -1) f (- x o))))))))
  (define (node1 obj)
   (car obj))
  (define (node2 obj)
   (cadr obj))
  (define (freq obj)
   (if (< (length obj) 3) 1 (third obj)))
  (define (envelope1 obj)
   (if (< (length obj) 5) (lambda (x) 1) (read-func (fifth obj) head)))
  (define (envelope2 obj)
   (if (< (length obj) 6) (let [[e1 (envelope1 obj)]] (lambda (x) (- (e1 x)))) (read-func (sixth obj) head)))
  (define (offset obj)
   (if (< (length obj) 4) 0 (fourth obj)))))
