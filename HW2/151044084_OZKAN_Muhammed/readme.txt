Flex kýsmý için aþaðýdaki adýmlarý konsol üzerinde yaparak test edebilirsiniz. 
code.gpp kýsmýný istediðiniz test kodu ile deðiþtirebilirsiniz.

	flex gpp_lexer.l
	gcc gpp_lexer.c  -o lexer -lfl
	./lexer code.gpp


Lisp kýsmý için aþaðýdaki adýmlarý konsol üzerinde yaparak test edebilirsiniz. 
Program çalýþtýktan sonra kod isteme kýsmýnda code.gpp kýsmýný istediðiniz test kodu ile deðiþtirebilirsiniz.

	clisp gpp_lexer.lisp
	Please entered a code file name : code.gpp






