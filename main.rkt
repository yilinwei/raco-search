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

(require raco/command-name
         "search.rkt"
         "index.rkt"
         "format.rkt"
         levenshtein)

(when (current-command-name)
  (define-values (search-term num-results)
    (command-line
     #:program (short-program+command-name)
     #:args (search-term (num-results 10))
     (values search-term num-results)))

  (define results (search
                   (curry levenshtein search-term)
                   plt-search-data
                   num-results))
  (for
      ([result (in-list results)])
    (displayln (~e result))))

(module+ test
  (require rackunit
           (for-syntax "js.rkt"))
  (include/reader "test.js" read-syntax)
  (check-equal? 42 x)
  (check-true (pair?
               (search
                (curry levenshtein
                       "set!")
                plt-search-data
                1))))
