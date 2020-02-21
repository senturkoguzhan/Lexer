;;;;;;;;;;;;;Oğuzhan ŞENTÜRK;;;;;;;;;;;;;
;;;;;;;;;;;osenturk@gtu.edu.tr;;;;;;;;;;;

(defun gppinterpreter (filename)
	(setq empty_list nil)	; EMPTY LIST
	(setq digit_flag 0)		; DIGIT CHECK
	(setq list_flag 0)		; EMTY LIST CHECK
	(setq star_flag 0)		; MULT OR DBMULT CHECK
	(setq comment_flag 0)	; COMMENT CHECK
	(let ((input(open filename :if-does-not-exist nil))) ; READ FILE CHAR BY CHAR
		(when input (loop for ch = (read-char input nil) while ch do
						(cond
							((equal ch #\;)
								(incf comment_flag) ; ; COUNT ++
								(when 
									(equal comment_flag 2) ; OUR GRAMMAR COMMNET IS ;;
									(print "COMMENT")
									(loop for ch = (read-char input nil) while (not(equal ch #\newline))))) ; COMMENT LINE UNTIL NEXT LINE
							((equal  ch  #\( )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
									 	(is_keyword empty_list))
									(setq star_flag 0)
									(setq empty_list nil ) 
									(setq list_flag 0 ) 
									(setq digit_flag 0)
									(is_operator ch))	; PRINT OPERATOR
							((equal  ch  #\) )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
									(setq star_flag 0)
									(setq empty_list nil )
									(setq list_flag 0 ) 
									(setq digit_flag 0)
									(is_operator ch)) ; PRINT OPERATOR
							((equal  ch  #\+ )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
									(setq star_flag 0)
									(setq empty_list nil )
									(setq list_flag 0 ) 
									(setq digit_flag 0)
									(is_operator ch))	; PRINT OPERATOR	 		
							((equal  ch  #\- )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list))
									(setq star_flag 0)
									(setq empty_list nil )	
									(setq list_flag 0 ) 
									(setq digit_flag 0)
									(is_operator ch))		 ; PRINT OPERATOR	
							((equal  ch  #\/ )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
									(setq star_flag 0)
									(setq empty_list nil )
									(setq list_flag 0 )
									(setq digit_flag 0)
									(is_operator ch)) ; PRINT OPERATOR
							((equal  ch  #\“ )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
									(setq star_flag 0)
									(setq empty_list nil )
									(setq list_flag 0 )
									(setq digit_flag 0)
									(is_operator ch)) ; PRINT OPERATOR
							((equal  ch  #\” )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
									(setq star_flag 0)
									(setq empty_list nil )
									(setq list_flag 0 )
									(setq digit_flag 0)
									(is_operator ch))	 ; PRINT OPERATOR
							((equal  ch  #\, )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
									(setq star_flag 0)
									(setq empty_list nil )
									(setq list_flag 0 )
									(setq digit_flag 0)
									(is_operator ch))	 ; PRINT OPERATOR								
							((equal  ch  #\* )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list))
									(incf star_flag)	; STAR COUNT ++
									(setq empty_list nil )	
									(setq list_flag 0 ) 
									(setq digit_flag 0)) ; I DONT PRINT OPERATOR HERE BECAUSE I DONT KNOW * OR ** 
							((equal  ch  #\space )
								(when	
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
								(is_star star_flag)	; PRINT * OR ** OPERATOR
								(setq star_flag 0) 
								(setq empty_list nil )	
								(setq list_flag 0 ) 
								(setq digit_flag 0))			
							((equal  ch  #\newline )
								(when
									(> list_flag 0) ; IF LIST IS NOT EMPTY PASS LIST IS KEYWORD FUNCTION
										(is_keyword empty_list)) 
								(setq star_flag 0)
								(setq empty_list nil )	
								(setq list_flag 0 ) 
								(setq digit_flag 0))

							(t
								(cond  ; if char is not operaotor char is digit or letter
									(( is_digit ch digit_flag) ; CHECK DIGIT
										(setq star_flag 0)	
										(setq digit_flag 1))
									((is_letter ch)		; CHECK LETTER
										(setq list_flag 1 )
										(setq empty_list (concatenate 'string empty_list (list ch))))))))))) ; CONCAT CHAR

(defun is_operator(operator)
	(cond 							
		((equal  operator  #\( )
			(print "OP_OP"))
		((equal  operator  #\) ) 
			(print "OP_CP"))
		((equal  operator  #\+ )
			(print "OP_PLUS"))
		((equal  operator  #\- )
			(print "OP_MINUS"))
		((equal  operator  #\/ )
			(print "OP_DIV"))
		((equal  operator  #\“ ) 
			(print "OP_OC"))
		((equal  operator  #\” )
			(print "OP_CC"))
		((equal  operator  #\, )
			(print "OP_COMMA"))
		(t
			nil))
)

(defun is_keyword(keyword)
	(cond 
		((equal keyword  "and")
			(print "KW_AND"))
		((equal  keyword  "or")
			(print "or"))
		((equal  keyword  "not")
			(print "KW_NOT"))
		((equal  keyword  "equal")
			(print "KW_EQUAL"))
		((equal  keyword "less")
			(print "KW_LESS"))
		((equal  keyword  "nil")
			(print "KW_NIL"))
		((equal  keyword  "list")
			(print "KW_LIST"))
		((equal  keyword  "append")
			(print "KW_APPEND"))
		((equal  keyword  "concat")
			(print "KW_CONCAT"))
		((equal  keyword  "set" )
			(print "KW_SET"))
		((equal keyword  "deffun")
			(print "KW_DEFFUN"))
		((equal  keyword  "for" )
			(print "KW_FOR"))
		((equal  keyword  "if")
			(print "KW_IF"))
		((equal  keyword  "exit")	
			(print "KW_EXIT"))
		((equal  keyword  "load")
			(print "KW_LOAD"))
		((equal  keyword "disp")
			(print "KW_DISP"))
		((equal  keyword  "true")
			(print "KW_TRUE"))
		((equal  keyword  "false")
			(print "KW_FALSE"))
		(t
			(print "IDENTIFIER")))			
)

(defun is_letter(letter)		
	(if (alpha-char-p letter) ; CHECK LETTER
		t
		nil)
)

(defun is_digit(digit digit_flag)
	(if (and(digit-char-p digit) (equal digit_flag 0)) ;CHECK DIGIT IF CHAR IS DIGIT AND DIGIT NOT PRINTIN UNTIL NOW PRINT DIGIT
		(print "VALUE")
		nil)

)

(defun is_star(star)
	(cond
		((= star 2)
			(print "OP_DBLMULT"))
		((= star 1)
			(print "OP_MULT"))
		(t		
			nil))
)	

(gppinterpreter "helloworld.g++") 