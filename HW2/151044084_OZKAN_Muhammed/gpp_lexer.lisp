;**********************************
;* CSE341 - Programming Languages *
;*                                *
;* 151044084 Muhammed ÖZKAN	      *
;* 		                    	  *
;**********************************

(defun read-code-helper (input output flag)	
	(let ((char (read-char input NIL)))
		(unless (NULL char)
			(when (equal char #\;)
				(format output "~c" char)
				(incf flag))
				(when (equal flag 0)
				(format output "~c" char))
				(when (equal char #\-)
				(format output "~c" #\Space))
			(when (and (equal char #\Newline) (equal flag 2))
				(format output "~c" char)
				(decf flag)
				(decf flag))
			(when (equal flag 2)
				(format output "~c" #\Space))
		  (read-code-helper input output flag))))

(defun read-code (codefile)
	(with-open-file (str codefile)
	  	(with-output-to-string (str-Output)
		    (read-code-helper str str-Output 0))))

(defun deleteSpace (in out)
	(setq firstElement (car in))
	(if (equal firstElement NIL)
		out
		(if (equal firstElement "")
			(deleteSpace (cdr in) out)
			(progn 
				(setq out (append out(list firstElement)))
				(deleteSpace (cdr in) out)))))

(defun tokenizer (string &optional (separator '(#\Space #\Tab #\Newline)))
	(deleteSpace (tokenizer-helper  string separator) '()))

(defun tokenizer-helper (string &optional (separator '(#\Space #\Tab #\Newline)) (r NIL))
  	(let ((n (position separator string :from-end t :test #'(lambda (x y)  (find y x :test #'string=)))))
	    (if n
			(tokenizer-helper  (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
		    	(cons string r))))

(defun typeCheckControl (returnList lexerL)
	(setq token (car lexerL))
	(if (equal token NIL)
		returnList
		(if (equal token "and")
			(progn
				(setq returnList (append returnList (list (list "KW_AND" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "or")
			(progn
				(setq returnList (append returnList (list (list "KW_OR" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "not")
			(progn
				(setq returnList (append returnList (list (list "KW_NOT" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "equal")
			(progn
				(setq returnList (append returnList (list (list "KW_EQUAL" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "less")
			(progn
				(setq returnList (append returnList (list (list "KW_LESS" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "nil")
			(progn
				(setq returnList (append returnList (list (list "KW_NIL" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "list")
			(progn
				(setq returnList (append returnList (list (list "KW_LIST" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "append")
			(progn
				(setq returnList (append returnList (list (list "KW_APPEND" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "concat")
			(progn
				(setq returnList (append returnList (list (list "KW_CONCAT" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "set")
			(progn
				(setq returnList (append returnList (list (list "KW_SET" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "deffun")
			(progn
				(setq returnList (append returnList (list (list "KW_DEFFUN" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "for")
			(progn
				(setq returnList (append returnList (list (list "KW_FOR" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "if")
			(progn
				(setq returnList (append returnList (list (list "KW_IF" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "exit")
			(progn
				(setq returnList (append returnList (list (list "KW_EXIT" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "load")
			(progn
				(setq returnList (append returnList (list (list "KW_LOAD" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "disp")
			(progn
				(setq returnList (append returnList (list (list "KW_DISP" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "true")
			(progn
				(setq returnList (append returnList (list (list "KW_TRUE" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "false")
			(progn
				(setq returnList (append returnList (list (list "KW_FALSE" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "+")
			(progn
				(setq returnList (append returnList (list (list "OP_PLUS" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "-")
			(progn
				(setq returnList (append returnList (list (list "OP_MINUS" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "/")
			(progn
				(setq returnList (append returnList (list (list "OP_DIV" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "*")
			(progn
				(setq returnList (append returnList (list (list "OP_MULT" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "(")
			(progn
				(setq returnList (append returnList (list (list "OP_OP" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token ")")
			(progn
				(setq returnList (append returnList (list (list "OP_CP" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "**")
			(progn
				(setq returnList (append returnList (list (list "OP_DBLMULT" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "“")
			(progn
				(setq returnList (append returnList (list (list "OP_OC" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token "”")
			(progn
				(setq returnList (append returnList (list (list "OP_CC" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token ",")
			(progn
				(setq returnList (append returnList (list (list "OP_COMMA" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (equal token ";;")
			(progn
				(setq returnList (append returnList (list (list "COMMENT" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (isIdentifier (concatenate 'list token))
			(progn
				(setq returnList (append returnList (list (list "IDENTIFIER" token))))
				(typeCheckControl returnList (cdr lexerL)))
			(if (isInteger (concatenate 'list (concatenate 'list token)))
			(progn
				(setq returnList (append returnList (list (list "VALUE" token))))
				(typeCheckControl returnList (cdr lexerL)))
							token)))))))))))))))))))))))))))))))))

(defun check-PAS (returnStr readStrList)
	(setq firstElement (car readStrList))
	(if (equal firstElement NIL)
		returnStr
		(if (equal (string firstElement) "(")
			(progn
				(setq returnStr (concatenate 'string returnStr (string firstElement)))
				(setq returnStr (concatenate 'string returnStr " "))
				(check-PAS returnStr (cdr readStrList)))
			(if (equal (string firstElement) ")")
				(progn
					(setq returnStr (concatenate 'string returnStr " "))
					(setq returnStr (concatenate 'string returnStr (string firstElement)))
					(check-PAS returnStr (cdr readStrList)))
				(progn
					(setq returnStr (concatenate 'string returnStr  (string firstElement)))
					(check-PAS returnStr (cdr readStrList)))))))

(defun isIdentifier (strList)
	(setq val (car strList))
	(if (equal val NIL)
		T
		(if (or (and (<= (char-code #\a) (char-code val)) (<= (char-code val) (char-code #\z))) (and (<= (char-code #\A) (char-code val)) (<= (char-code val) (char-code #\Z))))
			(isIdentifier (cdr strList))
			NIL)))

(defun isInteger (posList)
	(setq val (car posList))
	(if (equal val NIL)
		T
		(if (and (<= (char-code #\0) (char-code val)) (<= (char-code val) (char-code #\9)))
			(isInteger (cdr posList))
			NIL)))

(defun printList (pList)
	(setq val (reverse (car pList)))
	(if (not (equal val NIL))
		(progn
			(princ (nth 0 val))
			(princ #\Tab)
			(princ (nth 1 val))
			(terpri)
			(printList (cdr pList)))))

(defun lexer (codefile)
	(setq str (read-code codefile))   
	(setq lexerList (tokenizer (check-PAS "" (concatenate 'list str)))) 
	(setq outList (typeCheckControl '() lexerList)) 	
	(if (listp outList)	
		(printList outList)	
		(progn 
			(write-string "SYNTAX_ERROR ")
			(princ outList)
			(write-string " cannot be tokenized"))))

(defun gppinterpreter ()
	(write-string "Please entered a code file name : ") 
	(lexer (read-line)) 
)

(gppinterpreter)