#lang racket

(require
 (for-syntax "js.rkt"))

(include/reader "plt-index.js" read-syntax)

(provide (rename-out
          [plt_main_url plt-main-url]
          [plt_span_classes plt-span-classes]
          [plt_search_data plt-search-data]))
