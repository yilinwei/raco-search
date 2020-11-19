#lang info

(define collection "raco-search")
(define version "0.1")
(define deps '("base" "levenshtein"))
(define pre-install-collection "installer.rkt")
(define raco-commands
  '(("search" raco-search/main "search" 10)))
