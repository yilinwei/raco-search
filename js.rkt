#lang racket

(require syntax/strip-context
         "parse.rkt")
 
(provide (rename-out
          [read/js read]
          [read-syntax/js read-syntax]))
 
(define (read/js in)
  (syntax->datum
   (read-syntax/js #f in)))
 
(define (read-syntax/js src in)
  (if (eq? (peek-char in) eof)
   eof
   (with-syntax ([(body ...)
                  (parse (λ () (lex in)))])
     (strip-context
      #'(begin body ...)))))

(module+ test
  (require rackunit)
  (check-not-false
   (read/js (open-input-file "test.js"))))
