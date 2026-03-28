(define-library (vectr read-note)
 (import (scheme base) (vectr parse-freq) (scheme char) (scheme read) (srfi 1))
 (export read-notes)
 (begin
  (define (read-note head port)
   (call-with-port
    (lambda (p)
     (define freq-data
      (let [[f-data (read p)]]
       (cond
        [(symbol? f-data) (parse-freq (symbol->string f-data) head)]
	[(pair? f-data) (cons* (parse-freq (symbol->string (first f-data)) head)
                               (parse-freq (symbol->string (second f-data)) head)
                               (cddr f-data))])))
     (let loop [[res (list (cons 'freq freq-data))]
                [next (read-char p)]]
      (cond
       [(eof-object? next) res]
       [(char-whitespace? next) (loop res (read-char p))]
       [else (let [[data (read p)]] (if (eof-object? data) (error "EOL while reading flags" p) (loop (alist-cons next data res) (read-char p))))])))
    (open-input-string (read-line port))))))
