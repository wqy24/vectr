; Transfer 24-bit ints into bytevector with s24_LE format
(define-library (vectr write-sample)
 (import (scheme base) (wqy24 assert) (srfi 1))
 (export write-samples!)
 (begin
  (define (write-samples! datas port)
   (write-bytevector
    (apply bytevector
     (fold
      (lambda (data next)
       (assert data integer? "Data must be an integer")
       (assert data (lambda (x) (<= -8388608 x 8388607)) "Data must be s24")
       (cons*
        (remainder data 256)
        (remainder (quotient data 256) 256)
        (quotient data 65536) next)) '() datas)) port))))
