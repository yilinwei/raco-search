#lang racket

(require setup/dirs)

(define (pre-installer _)
  (define index (build-path (find-doc-dir) "search" "plt-index.js"))
  (make-file-or-directory-link index "plt-index.js"))

(provide pre-installer)
