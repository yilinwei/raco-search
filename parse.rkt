#lang racket

(require
 parser-tools/lex
 parser-tools/yacc
 (prefix-in : parser-tools/lex-sre))

(define-empty-tokens
  tokens-0
  (EOF
   VAR
   TRUE
   FALSE
   SEMICOLON
   COMMA
   COLON
   ASSIGN
   LBRACKET
   RBRACKET
   LCURLY
   RCURLY
   LSQUARE
   RSQUARE))

(define-tokens
  tokens-1
  (NUM SYM STRING))

(define (lex/string in)
  (define (read escape)
    (define char (read-char in))
    (cond
      [(and (eq? char #\\)
            (not escape))
       (read #t)]
      [(and (eq? char #\") (not escape))
       '()]
      [else (cons char (read #f))]))
  (token-STRING
   (list->string
    (read #f))))

(define (skip in)
  (define char (read-char in))
  (cond
    [(eq? char #\newline) (lex in)]
    [else (skip in)]))

(define lex
  (lexer
   [(eof) 'EOF]
   [#\" (lex/string input-port)]
   [whitespace (lex input-port)]
   [#\: 'COLON]
   ["//" (skip input-port)]
   [#\; 'SEMICOLON]
   [#\, 'COMMA]
   [#\{ 'LCURLY]
   [#\} 'RCURLY]
   [#\( 'LBRACKET]
   [#\) 'RBRACKET]
   [#\[ 'LSQUARE]
   [#\] 'RSQUARE]
   [#\} 'RCURLY]
   [#\. 'DOT]
   [#\= 'ASSIGN]
   [(:or "var" "true" "false")
    (string->symbol
     (string-upcase lexeme))]
   [(:+ numeric)
    (token-NUM (string->number lexeme))]
   [(::
     (:or alphabetic #\_)
     (:* alphabetic numeric #\_))
    (token-SYM lexeme)]))

(match-define
  [list parse/statement parse/block]
  (parser
   (tokens
    tokens-0
    tokens-1)
   (start
    stmt
    block)
   (end EOF)
   (error (λ (tok-ok? tok-name tok-value)
            (error (format "~a ~a ~a" tok-ok? tok-name tok-value))))
   (grammar
    (stmt
     [(expr SEMICOLON) $1]
     [(VAR SYM ASSIGN expr SEMICOLON)
      `(define ,(string->symbol $2) ,$4)])

    (array-1
     [(RSQUARE) '()]
     [(COMMA expr array-1)
      (cons $2 $3)])
    (array
     [(LSQUARE expr array-1) `(list->vector (list ,@(cons $2 $3)))]
     [(LSQUARE RSQUARE) (make-vector 0)])

    (dict-key
     [(expr COLON expr)
      `(cons ,$1 ,$3)])
    (dict-1
     [(RCURLY) '()]
     [(COMMA dict-key dict-1)
      (cons $2 $3)])
    (dict
     [(LCURLY RCURLY) (make-hash)]
     [(LCURLY dict-key dict-1)
      `(make-hash (list ,@(cons $2 $3)))])

    (apply-0
     [(RBRACKET) '()]
     [(COMMA expr)
      (cons $2)])

    (funcall
     [(SYM LBRACKET expr apply-0)
      `(,(string->symbol $1) ,@(cons $3 $4))])

    (expr
     [(SYM LSQUARE expr RSQUARE)
      `(vector-ref ,(string->symbol $1) ,$3)]
     [(STRING) $1]
     [(TRUE) #t]
     [(dict) $1]
     [(FALSE) #f]
     [(num) $1]
     [(array) $1]
     [(funcall) $1]
     [(SYM)
      (string->symbol $1)])
    (num
     [(NUM) $1])
    (block
     [(stmt) (list $1)]
     [(stmt block)
      (cons $1 $2)]))))

(provide
 lex
 (rename-out [parse/block parse]))

(module+ test
  (require rackunit)

  (define (parse/string str)
    (define in (open-input-string str))
    (parse/statement [λ () (lex in)]))

  (check-equal? (parse/string "foo;") 'foo)
  (check-equal? (parse/string "[foo, [bar]];") '(list->vector (list foo (list->vector (list bar)))))
  (check-equal? (parse/string "var foo = bar;") '(define foo bar))
  (check-equal? (parse/string "foo[0];") '(vector-ref foo 0))
  (check-equal? (parse/string "var foo = \"bar\";") '(define foo "bar"))
  (check-equal? (parse/string "{ \"foo\": bar };") '(make-hash (list (cons "foo" bar))))
  (check-eq? (parse/string "true;") #t)
  (check-eq? (parse/string "//comments\ntrue;") #t)
  (check-equal? (parse/string "f(1);") '(f 1)))
