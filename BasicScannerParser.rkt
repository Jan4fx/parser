#lang racket
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/lex)
(require parser-tools/lex-sre)

;Tokenizes the input file
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
   

   [(:or "$$")
        (cons `$$
              (tokenizer input-port))]
   
   ;(exact-positive-integer? 1)
   ;0200 cannot be a line number
   ;keep track of 0 and negative numbers
   
   [(:: (:? #\-)(:+ (char-range #\0 #\9)))
        (cons (string->number lexeme)
             (tokenizer input-port))]

   [(:+ numeric) (string->number lexeme)]

   ;[(:: (:? #\-)(:+ (char-range #\0 #\9)))
    ;(cond
     ; [(exact-positive-integer? #t (cons `INT (tokenizer input-port)))]
      ;[(exact-positive-integer? #f (cons 'NONPOSITIVEINT (tokenizer input-port)))])]

   [whitespace (tokenizer input-port)]
   ))


;Keeps track of line number
;if error is on linenumber -1 then the first line didn't have a linenumber
(define linenumber -1) 


;Starts the parse and every subsequent line
(define (program tokens)
    (cond
      ;every line has to start with linenumber or EOF symbol
      [(number?(first tokens)) (set! linenumber (first tokens)) (stmt_list (rest tokens))]
      [(equal? (first tokens)`newline) (program (rest tokens))]
      [(equal? (first tokens)`$$) (print"accept")]
      [else (error"Syntax Error on line " linenumber ": Expected Line Number or EOF; Instead got:" (first tokens))]))


(define (goidx tokens)
  (cond
    [(number?(first tokens)) (gotail (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected INT for goto/gosub; Instead got:" (first tokens))]))
    
(define (gotail tokens)
    (cond
    [(equal? (first tokens)`newline) (program (rest tokens))]
    [(equal? (first tokens)`if) (ifstmt (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected newline or if stmt; Instead got:" (first tokens))]))

(define (readidx tokens)
    (cond
    [(equal? (first tokens)'ID) (readtail (rest tokens))]
    [(number?(first tokens)) (readtail (rest tokens))]
    [(equal? (first tokens)'LPAR) (parstmt (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected ID INT or LPAR for read/write; Instead got:" (first tokens))]))

(define (parstmt tokens)
    (cond
    [(equal? (first tokens)'ID) (parexpr (rest tokens))]
    [(number?(first tokens)) (parexpr (rest tokens))]
    [(equal? (first tokens)'RPAR) (readtail (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected ID/INT/RPAR; Instead got:" (first tokens))]))

(define (parexpr tokens)
    (cond
    [(equal? (first tokens)'+) (parstmt (rest tokens))]
    [(equal? (first tokens)'-) (parstmt (rest tokens))]
    [(equal? (first tokens)'RPAR) (readtail (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected + - or RPAR; Instead got:" (first tokens))]))

(define (readtail tokens)
    (cond
    [(equal? (first tokens)`newline) (program (rest tokens))]
    [(equal? (first tokens)`+) (readidx (rest tokens))]
    [(equal? (first tokens)`-) (readidx (rest tokens))]
    [(equal? (first tokens)`=) (readidx (rest tokens))]
    [(equal? (first tokens)`:) (returnstmt (rest tokens))]
    [(equal? (first tokens)`if) (ifstmt (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected newline op or if stmt; Instead got:" (tokens))]))

(define (returnstmt tokens)
    (cond
    [(equal? (first tokens)`return) (returntail (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected return; Instead got:" (first tokens))]))

(define (returntail tokens)
    (cond
    [(equal? (first tokens)`newline) (program (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected newline; Instead got:" (first tokens))]))

(define (ifstmt tokens)
    (cond
    [(equal? (first tokens)'ID) (iftail (rest tokens))]
    [(number?(first tokens)) (iftail (rest tokens))]
    [(equal? (first tokens)'LPAR) (ifparstmt (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected ID or INT; Instead got:" (first tokens))]))

(define (iftail tokens)
    (cond
    [(equal? (first tokens)'+) (ifstmttail (rest tokens))]
    [(equal? (first tokens)'-) (ifstmttail (rest tokens))]
    [(equal? (first tokens)'=) (ifstmttail (rest tokens))]
    [(equal? (first tokens)'then) (stmt_list (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected op; Instead got:" (first tokens))]))

(define (ifstmttail tokens)
    (cond
    [(equal? (first tokens)'ID) (iftail (rest tokens))]
    [(number?(first tokens)) (iftail (rest tokens))]
    [(equal? (first tokens)'LPAR) (ifparstmt (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected math statement; Instead got:" (first tokens))]))

(define (ifparstmt tokens)
    (cond
    [(equal? (first tokens)'ID) (ifparexpr (rest tokens))]
    [(number?(first tokens)) (ifparexpr (rest tokens))]
    [(equal? (first tokens)'RPAR) (thenstmt (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected ID/INT/RPAR; Instead got:" (first tokens))]))

(define (ifparexpr tokens)
    (cond
    [(equal? (first tokens)'+) (ifparstmt (rest tokens))]
    [(equal? (first tokens)'-) (ifparstmt (rest tokens))]
    [(equal? (first tokens)'RPAR) (thenstmt (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected op or RPAR; Instead got:" (first tokens))]))

(define (thenstmt tokens)
    (cond
    [(equal? (first tokens)'newline) (thenstmt (rest tokens))]
    [(equal? (first tokens)'then) (stmt_list (rest tokens))]
    [else (error"Syntax Error On Line" linenumber "Expected then statement; Instead got:" (tokens))]))

(define (stmt_list tokens)
    (cond
      
      [(equal? (first tokens)`read) (readidx (rest tokens))]
      [(equal? (first tokens)`write) (readidx (rest tokens))]
      [(equal? (first tokens)`goto) (goidx (rest tokens))]
      [(equal? (first tokens)`gosub) (goidx (rest tokens))]
      [(equal? (first tokens)`ID) (readtail (rest tokens))]
      [(equal? (first tokens)`if) (ifstmt (rest tokens))]
      [else (error "Syntax Error On Line" linenumber " Expected stmt; Instead got:" (first tokens))]))


(define tokens(tokenizer(open-input-file "file01.txt"))) ;test token stream

;calls the parser after calling the scanner
(define (parse input-file)
  (program (tokenizer (open-input-file input-file)))
)

;test parse function

;(parse "file01.txt")
;(parse "file02.txt")
;(parse "file03.txt")
;(parse "file04.txt")
;(parse "file05.txt")
;(parse "file06.txt")
;(parse "file07.txt")
