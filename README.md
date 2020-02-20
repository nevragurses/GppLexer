Given the description of the G++ language in  G++Syntax.pdf.
I implemented a lexer that does the tokenization of any set of valid G++ expressions or statements.  
Lexical analysis rules of G++ is explained in the first 4 pages of G++Syntax.pdf. 
Lexically incorrect statements or expressions should be identified, and an error should be generated. 
I implemented the lexer in two different ways: 
1. There are tools to implement lexers given the rules in a meta-grammar such as CFGs. One such tool is “Flex” that lets me generate C code to do the lexical analysis.  
2. For this project, I implemented a lexer for G++ in Lisp. For this, I don't  use a meta-grammar to define the lexical syntax of your language.  
Both lexers  start the interpreter. It read one line at a time from the user and check if the input lexically correct while generating the tokens for later processing.

All of rules about project is explaned in Homework 2 pdf file.

To use these lexers, follow these instructions:

a)For clisp lexer: Run the program like this: "clisp gpp_lexer.lisp"
b)For flex lexer:  Firstly: "lex gpp_lexer.l"
		   After: "gcc lex.yy.c"
		   Then: "./a.out"

2-Dollar sign($)  come to screen.

3-Then if you want run file lexer you should enter "g++ filename .g++" .
or if you want run terminal lexer you should enter only "g++"
