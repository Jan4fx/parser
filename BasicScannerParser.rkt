#lang racket
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/lex)
(require parser-tools/lex-sre)

(define tokenizer
  (lexer
   [(eof) '()]

   [(:or "goto")
    (cons `goto
          (tokenizer input-port))]
   
   [(:or "gosub")
    (cons `gosub
          (tokenizer input-port))]
   
   [(:or "write")
    (cons `write
          (tokenizer input-port))]
   
   [(:or "read")
    (cons `read
          (tokenizer input-port))]
   
   [(:or "if")
    (cons `if
          (tokenizer input-port))]
   
   [(:or "\n")
    (cons `newline
          (tokenizer input-port))]
   
   [(:or "then")
    (cons `then
          (tokenizer input-port))]
   
   [(:or "return")
    (cons `return
          (tokenizer input-port))]
     
   [(:+ (:or (char-range #\a #\z)(char-range #\A #\Z)))
    (cons `ID
              (tokenizer input-port))]
   [#\(
    (cons `LPAR
          (tokenizer input-port))]
   [#\)
    (cons `RPAR
          (tokenizer input-port))]
   [#\-
    (cons `-
          (tokenizer input-port))]
   [#\+
    (cons `+
     (tokenizer input-port))]
   
   [#\*
    (error"Error Unknown Operator While Scanning: *")]
   
   [#\/
    (error"Error Unknown Operator While Scanning: /")]

   [#\=
    (cons `=
     (tokenizer input-port))]

    [#\:
    (cons `:
     (tokenizer input-port))]
   

   [(:+ (:or #\$ #\$))
        (cons `$$
              (tokenizer input-port))]
   [#\$
    (cons `(END, (string->symbol lexeme))
          (tokenizer input-port))]
   
   [(:: (:? #\-)(:+ (char-range #\0 #\9)))
        (cons `INT
             (tokenizer input-port))]

   [whitespace (tokenizer input-port)]
   ))

;Program
(define (program tokens)
    (cond
      ;every line has to start with linenumber or EOF symbol
      [(equal? (first tokens)`INT) (stmt_list (rest tokens))]
      [(equal? (first tokens)`newline) (program (rest tokens))]
      [(equal? (first tokens)`$$) (print"accept")]
      [else (error"Syntax Error: Expected Line Number or EOF; Instead got:" (first tokens))]))


(define (goidx tokens)
  (cond
    [(equal? (first tokens)`INT) (gotail (rest tokens))]
    [else (error"Syntax Error: Expected INT for goto/gosub; Instead got:" (first tokens))]))
    

(define (gotail tokens)
    (cond
    [(equal? (first tokens)`newline) (program (rest tokens))]
    [(equal? (first tokens)`if) (ifstmt (rest tokens))]
    [else (error"Syntax Error: Expected newline or if stmt; Instead got:" (first tokens))]))

(define (readidx tokens)
    (cond
    [(equal? (first tokens)'ID) (readtail (rest tokens))]
    [(equal? (first tokens)'INT) (readtail (rest tokens))]
    [(equal? (first tokens)'LPAR) (parstmt (rest tokens))]
    [else (error"Syntax Error: Expected ID INT or LPAR for read/write; Instead got:" (first tokens))]))

(define (parstmt tokens)
    (cond
    [(equal? (first tokens)'ID) (parexpr (rest tokens))]
    [(equal? (first tokens)'INT) (parexpr (rest tokens))]
    [(equal? (first tokens)'RPAR) (readtail (rest tokens))]
    [else (error"Syntax Error: Expected ID/INT/RPAR; Instead got:" (first tokens))]))

(define (parexpr tokens)
    (cond
    [(equal? (first tokens)'+) (parstmt (rest tokens))]
    [(equal? (first tokens)'-) (parstmt (rest tokens))]
    [(equal? (first tokens)'RPAR) (readtail (rest tokens))]
    [else (error"Syntax Error: Expected + - or RPAR; Instead got:" (first tokens))]))



(define (readtail tokens)
    (cond
    [(equal? (first tokens)`newline) (program (rest tokens))]
    [(equal? (first tokens)`+) (readidx (rest tokens))]
    [(equal? (first tokens)`-) (readidx (rest tokens))]
    [(equal? (first tokens)`=) (readidx (rest tokens))]
    [(equal? (first tokens)`:) (returnstmt (rest tokens))]
    [(equal? (first tokens)`if) (ifstmt (rest tokens))]
    [else (error"Syntax Error: Expected newline op or if stmt; Instead got:" (tokens))]))

(define (returnstmt tokens)
    (cond
    [(equal? (first tokens)`return) (returntail (rest tokens))]
    [else (error"Syntax Error: Expected return; Instead got:" (first tokens))]))

(define (returntail tokens)
    (cond
    [(equal? (first tokens)`newline) (program (rest tokens))]
    [else (error"Syntax Error: Expected newline; Instead got:" (first tokens))]))

(define (ifstmt tokens)
    (cond
    [(equal? (first tokens)'ID) (iftail (rest tokens))]
    [(equal? (first tokens)'INT) (iftail (rest tokens))]
    [(equal? (first tokens)'LPAR) (ifparstmt (rest tokens))]
    [else (error"Syntax Error: Expected ID or INT; Instead got:" (first tokens))]))

(define (iftail tokens)
    (cond
    [(equal? (first tokens)'+) (ifstmttail (rest tokens))]
    [(equal? (first tokens)'-) (ifstmttail (rest tokens))]
    ;;;;;;
    [(equal? (first tokens)'=) (ifstmttail (rest tokens))]
    [(equal? (first tokens)'then) (stmt_list (rest tokens))]
    [else (error"Syntax Error: Expected op; Instead got:" (first tokens))]))



;;;;;;
(define (ifstmttail tokens)
    (cond
    [(equal? (first tokens)'ID) (iftail (rest tokens))]
    [(equal? (first tokens)'INT) (iftail (rest tokens))]
    [(equal? (first tokens)'LPAR) (ifparstmt (rest tokens))]
    [else (error"Syntax Error: Expected math statement; Instead got:" (first tokens))]))

(define (ifparstmt tokens)
    (cond
    [(equal? (first tokens)'ID) (ifparexpr (rest tokens))]
    [(equal? (first tokens)'INT) (ifparexpr (rest tokens))]
    [(equal? (first tokens)'RPAR) (thenstmt (rest tokens))]
    [else (error"Syntax Error: Expected ID/INT/RPAR; Instead got:" (first tokens))]))

(define (ifparexpr tokens)
    (cond
    [(equal? (first tokens)'+) (ifparstmt (rest tokens))]
    [(equal? (first tokens)'-) (ifparstmt (rest tokens))]
    [(equal? (first tokens)'RPAR) (thenstmt (rest tokens))]
    [else (error"Syntax Error: Expected op or RPAR; Instead got:" (first tokens))]))


;check for par
(define (thenstmt tokens)
    (cond
    [(equal? (first tokens)'newline) (thenstmt (rest tokens))]
    [(equal? (first tokens)'then) (stmt_list (rest tokens))]
    [else (error"Syntax Error: Expected then statement; Instead got:" (tokens))]))

(define (stmt_list tokens)
    (cond
      [(equal? (first tokens)`read) (readidx (rest tokens))]
      [(equal? (first tokens)`write) (readidx (rest tokens))]
      [(equal? (first tokens)`goto) (goidx (rest tokens))]
      [(equal? (first tokens)`gosub) (goidx (rest tokens))]
      [(equal? (first tokens)`ID) (readtail (rest tokens))]
      [(equal? (first tokens)`if) (ifstmt (rest tokens))]
      [else (error "Syntax Error: Expected stmt; Instead got:" (first tokens))]))

(define tokens(tokenizer(open-input-file "file01.txt"))) ;test token stream
;tokens

;parses input file
(define (parse input-file)
 (program (tokenizer (open-input-file input-file))))

;test parse function

;(parse "file01.txt")
;(parse "file02.txt")
;(parse "file03.txt")
;(parse "file04.txt")
;(parse "file05.txt")
;(parse "file06.txt")
;(parse "file07.txt")
