(define-library (wqy24 assert)
 (import (scheme base))
 (export assert)
 (begin
  (define (assert obj pred? msg)
   (unless (pred? obj)
    (error msg obj)))))
