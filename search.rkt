#lang racket

(define (less-than? % %%)
    (< (car %) (car %%)))

(define (search score-proc vec num)
  (for/fold
      ([results '()]
       [len 0]
       #:result
       (map cdr results))
      ([v (in-vector vec)])
    (define str (vector-ref v 0))
    (define rs
      (sort
       (cons
        (cons (score-proc str) v)
        results)
       less-than?))
    (if (> num len)
        (values rs (add1 len))
        (values (take rs num) len))))

(provide search)

(module+ test
  (require rackunit levenshtein)
  (check-equal?
   (search (curry levenshtein "foo")
           #(#("foo")
             #("bar")) 10)
   '(#("foo") #("bar"))))
