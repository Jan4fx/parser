Racket Lisp Top Down Parser and Scanner to run Syntax Checks on BASIC input files

>tokens 

will show token stream of "file03.txt" this is a hardcoded way to easily test the scanner over and over again

>(parse "file1.txt") 

will parse input file 1

Expected Outputs when running the parse function
File 1: Correct
File 2: Unknown operator; should get scan error on line 30.
File 3: Correct (at least, syntactically) 
File 4: Syntax error (repeated operator) on line 30. 
   The misspelled identifier in line 40 is NOT a syntax error. 
File 5: Syntax error (no line number)
File 6: Correct 

Provided Grammar 
program -> linelist $$ 
linelist -> line linelist | epsilon 
line -> idx stmt linetail* [EOL]
idx -> nonzero_digit digit* 
linetail -> :stmt | epsilon 
stmt -> id = expr | if expr then stmt | read id | write expr | goto idx | gosub idx | return
expr -> id etail | num etail | (expr)
etail -> + expr | - expr | = expr | epsilon
id -> [a-zA-Z]+
num -> numsign digit digit*
numsign -> + | - | epsilon 
nonzero_digit -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
digit -> 0 | nonzero_digit 

I plugged the provided grammar into http://hackingoff.com/compilers/predict-first-follow-set to create all of my needed first follow and predict sets

Used racket documentation to help me learn the basic fundamentals
