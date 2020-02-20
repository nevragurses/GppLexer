;Reads file.
(defun readFile (filename)
	(let ( (file (open filename :if-does-not-exist nil))(str (string "")) ) 
		(setq defineNewline 0)
		(when file
			(loop for inner = (read-line file nil) ;loop for reading each line until document ends.
			    while inner
					do 
					(if (and (equal(char inner 0 ) #\; ) (equal(char inner 1 ) #\; )) ;This control to determine comment line.
						(setq str (concatenate 'string str ";;" ) ) ;comment line is recorded as token ;;
						(setq str (concatenate 'string str inner)) ;takes each line in a string.
					)	
			)
			(close file) ;closing file.
		)
		(setq str (concatenate 'string str " ")) ;puts space end of string.
		str
	)
)
; Implementing lexer. 
(defun lexer (input)
	(let ( (flagValue 0) (flag 0) (flagIdentifier 0)(flagInner 0) (control 0) (controlVal 0))
		(setq input (string-downcase input))
		(setq list (list #\+ #\- #\/ #\* #\( #\) #\" #\, #\space ) ) ;to keep operators in list for some controls.
		(loop for i from 0 to (-(length input)1) ;loop until end of string
			do
		
			(setq symbol (char input i)) ;assign currently character in variable named symbol.
		
			;This conditions to determine operators.
			(when (and(equal symbol #\;) (equal (char input (+ i 1)) #\;))
				(write-line "COMMENT")
				(setq control 1)
			)
			(when (equal symbol #\+) 
				(write-line "OP_PLUS")
				(setq control 1)
			)
			(when (equal symbol #\-) 
				(write-line "OP_MINUS")
				(setq control 1)
			
			)
			(when (equal symbol #\/) 
				(write-line "OP_DIV")
				(setq control 1)
			
			)
			(when (and(equal symbol #\*)(< (+ i ) (length input) )(not (equal (char input (+ i 1))  #\*))(not (equal (char input (- i 1))  #\*)))
				(write-line "OP_MULT")
				(setq control 1)
			
			)
			(when (equal symbol #\( )  
				(write-line "OP_OP")
				(setq control 1)
			)
			(when (equal symbol #\) )  
				(write-line "OP_CP")
				(setq control 1)
			)
			(when (and (equal symbol #\*) (< (+ i 1) (length input) )(equal (char input (+ i 1))  #\*)) 
				(write-line "OP_DBLMULT")
				(setq control 1)
			
			)
			(when (and (equal symbol #\" ) )
				( cond ((equal flag 0) ;this conditon to determine " character whether oc or cc.
					(write-line "OP_OC")  
					(setq flag 1))
					( 
						(write-line "OP_CC") 
					    (setq flag 0))

					)
					(setq control 1)
			)	
			(when (equal symbol #\, )  
				(write-line "OP_COMMA")
				(setq control 1)
			)
			;This condition to determine value.
			(when (equal (isDigit symbol) t)
					;This condition determine 0 as value.
					(when (and (equal symbol #\0) (< (+ i 1) (length input) )(equal (isDigit (char input (+ i 1)) ) nil ) (equal (isDigit (char input (- i 1)) ) nil ))
						(write-line "VALUE")
						(setq control 1)
					)
					;This condition determine the start of value.	
					(when (and (not (equal symbol #\0)) (equal (isDigit (char input (- i 1)) ) nil )(equal (isAlphabetical (char input (- i 1)) ) nil ))
						(setq flagValue 1)
						(setq control 1)

					)
					;This condition determine value that no leading zero.
					(when (and  (equal flagValue 1 ) (< (+ i 1) (length input) )(equal (isDigit (char input (+ i 1)) ) nil ))
						(write-line "VALUE")
						(setq flagValue 0)
						(setq control 1)
					)	
					(when (equal flagValue 1)
						(setq control 1)
					)			
			)
			;This conditions determine keywords.For tall keywords I use subseq equivalence control and small keywords I use  each character equivelence control.
			;determine keyword and.
			(when (and(equal symbol #\a )  (< (+ i 3) (length input) )(equal (char input (+ i 1)) #\n )(equal (char input (+ i 2)) #\d )(not (equal (position (char input (+ i 3) ) list )nil )))
					(write-line "KW_AND")
					(setq flagIdentifier 1)
					(setq i (+ i 2))
					(setq control 1)
			)
			;determine keyword or.
			(when (and(equal symbol #\o )  (< (+ i 2) (length input) )(equal (char input (+ i 1)) #\r )(not (equal (position (char input (+ i 2) ) list )nil )))
					(write-line "KW_OR")
					(setq flagIdentifier 1)
					(setq i (+ i 1))
					(setq control 1)
			)
			;determine keyword not.
			(when (and(equal symbol #\n )  (< (+ i 3) (length input) )(equal (char input (+ i 1)) #\o )(equal (char input (+ i 2)) #\t )(not (equal (position (char input (+ i 3) ) list )nil )))
					(write-line "KW_NOT")
					(setq flagIdentifier 1)
					(setq i (+ i 2))
					(setq control 1)
			)
			;determine keyword equal.
			(when (and(equal symbol #\e ) (< (+ i 5) (length input) )(equal (subseq input (+ i 1) (+ i 5)) "qual") (not (equal (position (char input (+ i 5) ) list )nil )))
					(write-line "KW_EQUAL")
					(setq flagIdentifier 1)
					(setq i (+ i 4))
					(setq control 1)
			)
			;determine keyword less.
			(when (and(equal symbol #\l )  (< (+ i 4) (length input) )(equal (subseq input (+ i 1) (+ i 4)) "ess") (not (equal (position (char input (+ i 4) ) list )nil )))
					(write-line "KW_LESS")
					(setq flagIdentifier 1)
					(setq i (+ i 3))
					(setq control 1)
			)
			;determine keyword nil.
			(when (and(equal symbol #\n )  (< (+ i 3) (length input) )(equal (char input (+ i 1)) #\i )(equal (char input (+ i 2)) #\l )(not (equal (position (char input (+ i 3) ) list )nil )))
					(write-line "KW_NIL")
					(setq flagIdentifier 1)
					(setq i (+ i 2))
					(setq control 1)
			)
			;determine keyword list.
			(when (and(equal symbol #\l )  (< (+ i 4) (length input) )(equal (subseq input (+ i 1) (+ i 4)) "ist") (not (equal (position (char input (+ i 4) ) list )nil )))
					(write-line "KW_LIST")
					(setq flagIdentifier 1)
					(setq i (+ i 3))
					(setq control 1)
			)
			;determine keyword append.
			(when (and(equal symbol #\a )  (< (+ i 6) (length input) )(equal (subseq input (+ i 1) (+ i 6)) "ppend") (not (equal (position (char input (+ i 6) ) list )nil )))
					(write-line "KW_APPEND")
					(setq flagIdentifier 1)
					(setq i (+ i 5))
					(setq control 1)
			)
			;determine keyword concat.
			(when (and(equal symbol #\c )  (< (+ i 6) (length input) )(equal (subseq input (+ i 1) (+ i 6)) "oncat") (not (equal (position (char input (+ i 6) ) list )nil )))
					(write-line "KW_CONCAT")
					(setq flagIdentifier 1)
					(setq i (+ i 5))
					(setq control 1)
			)
			;determine keyword set.
			(when (and(equal symbol #\s )  (< (+ i 3) (length input) )(equal (char input (+ i 1)) #\e )(equal (char input (+ i 2)) #\t )(not (equal (position (char input (+ i 3) ) list )nil )))
					(write-line "KW_SET")
					(setq flagIdentifier 1)
					(setq i (+ i 2))
					(setq control 1)
					
			)
			;determine keyword deffun.
			(when (and(equal symbol #\d )  (< (+ i 6) (length input) )(equal (subseq input (+ i 1) (+ i 6)) "effun") (not (equal (position (char input (+ i 6) ) list )nil )))
					(write-line "KW_DEFFUN")
					(setq flagIdentifier 1)
					(setq i (+ i 6))
					(setq control 1)
			)
			;determine keyword for.
			(when (and(equal symbol #\f )  (< (+ i 3) (length input) )(equal (char input (+ i 1)) #\o )(equal (char input (+ i 2)) #\r )(not (equal (position (char input (+ i 3) ) list )nil )))
					(write-line "KW_FOR")
					(setq flagIdentifier 1)
					(setq i (+ i 2))
					(setq control 1)
			)
			;determine keyword if.
			(when (and(equal symbol #\i )  (< (+ i 2) (length input) )(equal (char input (+ i 1)) #\f )(not (equal (position (char input (+ i 2) ) list )nil )))
					(write-line "KW_IF")
					(setq flagIdentifier 1)
					(setq i (+ i 1))
					(setq control 1)
			)
			;determine keyword exit.
			(when (and(equal symbol #\e )  (< (+ i 4) (length input) )(equal (subseq input (+ i 1) (+ i 4)) "xit") (not (equal (position (char input (+ i 4) ) list )nil )))
					(write-line "KW_EXIT")
					(setq flagIdentifier 1)
					(setq i (+ i 3))
					(setq control 1)
			)
			;determine keyword load.
			(when (and(equal symbol #\l ) (< (+ i 4) (length input) ) (equal (subseq input (+ i 1) (+ i 4)) "oad") (not (equal (position (char input (+ i 4) ) list )nil )))
					(write-line "KW_LOAD")
					(setq flagIdentifier 1)
					(setq i (+ i 3))
					(setq control 1)
			)
			;determine keyword disp.
			(when (and(equal symbol #\d )  (< (+ i 4) (length input) )(equal (subseq input (+ i 1) (+ i 4)) "isp") (not (equal (position (char input (+ i 4) ) list )nil )))
					(write-line "KW_DISP")
					(setq flagIdentifier 1)
					(setq i (+ i 3))
					(setq control 1)
			)
			;determine keyword true.
			(when (and(equal symbol #\t )  (< (+ i 4) (length input) )(equal (subseq input (+ i 1) (+ i 4)) "rue") (not (equal (position (char input (+ i 4) ) list )nil )))
					(write-line "KW_TRUE")
					(setq flagIdentifier 1)
					(setq i (+ i 3))
					(setq control 1)
			)
			;determine keyword false.
			(when (and(equal symbol #\f ) (< (+ i 5) (length input) ) (equal (subseq input (+ i 1) (+ i 5)) "alse") (not (equal (position (char input (+ i 5) ) list )nil )))
					(write-line "KW_FALSE")
					(setq flagIdentifier 1)
					(setq i (+ i 4))
					(setq control 1)
			)
			;This condition determine identifiers.
			(when (and (equal flagIdentifier 0)(or(equal (isAlphabetical symbol) t ) (equal flagInner 1)))
				(setq control 1) 
				(when  (equal (isDigit(char input (+ i 1))) t) ;identifier can include digits.
						(setq flagInner 1)
						(setq control 1)

				)
				;This condition determine end of identifier.
				(when  (not (equal (position (char input (+ i 1) ) list )nil ))
					(write-line "IDENTIFIER")
					(setq flagInner 0)
					(setq control 1)
				)
			)
			;this condition prevent to give error for space ,newline, ; , tab.  
			(when (or(equal symbol #\space )(equal symbol #\newline)(equal symbol #\*)(equal symbol #\;)(equal symbol #\tab))
				(Setq control 1)
			) 
			;This condition for giving error message if false lexeme entered.
			(when (equal control 0)
					(format t "SYNTAX_ERROR ~a cannot be tokenized!" symbol)(terpri)
				(return nil)
			
			)
			(setq flagIdentifier 0)
			(setq control 0)		
		)	
	)
)
;This function to determine a character whether alphabetical or not.
(defun isAlphabetical (letter )
	(let ( (alphabet (list )))
		(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t  #\u  #\v  #\w  #\x #\y #\z) )
		(if (not (equal(position letter alphabet )nil ))
			t
			nil
		)		
	)
)	
;This function to determine a character digit or not.
(defun isDigit (letter )
	(let ( (digit (list )))
		(setq digit '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 ) )
		(if (not (equal(position letter digit )nil ))
			t
			nil
		)		
	)
)			
;(lexer (readFile "file.txt"))
 
;This is interpreter.If a file name is entered as function parameter,load file in interpreter.Or not write expressions on interpreter line by line.
(defun gppinterpreter (&optional nameofFile)
	(write-string "$ ")
	
    (when  (equal nameofFile nil) ;If file name not entered
       
        (write-line "g++") ;write g++ on screen and then  write expressions and make lexical analiysis until (exit) entered.
        	(loop 
		
			(setq interpreter ( read-line ))  ;read line from interpreter.
			( lexer interpreter ) ;makes lexical analysis.
			(if(equal interpreter "(exit)") 
				(write-line "PROGRAM IS TERMINATING...")
			)
		(when (equal interpreter "(exit)")  (return 0)))

    )
    (when  (not (equal nameofFile nil)) ;If file name entered interpreted.Load file and make lexical analiysis.
       
        (Setq i 0)
		(loop ;This loop for writing filename on interpreter.
			(setq i  (+ i 1))
			(when (equal i (-(length nameofFile ) 4)) (return ( setq name (subseq  nameofFile 0 i   )))) 
		)
		
        (format t "g++ ~a.g++" name)(terpri) ;write g++ filename.g++ on interpreter.
        (lexer (readFile nameofFile )) ;read file and call lexer function to make lexical analysis.


    )
)

(write-line "TESTING INTERPRETER WITH INPUT FILE...")
( gppinterpreter "inputFile.g++" )  (terpri)

(write-line "TESTING INTERPRETER WITH NO INPUT FILE...WRITE EXPRESSIONS LINE BY LINE..")(terpri)
( gppinterpreter ) 
