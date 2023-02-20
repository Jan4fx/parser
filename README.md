Parser and Scanner to run Syntax Checks on BASIC
I created a Racket Lisp Top Down Scanner and Parser to syntactically check some BASIC coded input files
>tokens 
will show token stream
>(parse "file01.txt") 
will parse input file 1

Expected Outputs when running the parse function
File 1: Correct
File 2: Unknown operator; should get scan error on line 30.
File 3: Correct (at least, syntactically) 
File 4: Syntax error (repeated operator) on line 30. 
   The misspelled identifier in line 40 is NOT a syntax error. 
File 5: Syntax error (no line number)
File 6: Correct 

I plugged the provided grammar into http://hackingoff.com/compilers/predict-first-follow-set to create all of my needed first follow and predict sets
Used racket documentation to help me learn the basic fundamentals
