;; Copyright (c) 2020 by Yilin Wei.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
