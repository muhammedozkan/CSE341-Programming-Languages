%option noyywrap

%{
	#include <stdio.h>

	void ret_print(char *token_type);
	void yyerror();
%}

ID 			[a-zA-Z_][a-zA-Z0-9_]*
ICONST		0|[1-9][0-9]*

%%

";;".*					{ printf(";;\tCOMMENT\n");  } 

"and"       		{ ret_print("KW_AND"); }
"or"       		{ ret_print("KW_OR"); }
"not"      		{ ret_print("KW_NOT"); }
"equal"       	{ ret_print("KW_EQUAL"); }
"less"       	{ ret_print("KW_LESS"); }
"nil"       		{ ret_print("KW_NIL"); }
"list"      	{ ret_print("KW_LIST"); }
"append"      	{ ret_print("KW_APPEND"); }
"concat"      	{ ret_print("KW_CONCAT"); }
"set"      		{ ret_print("KW_SET"); }
"deffun"    	{ ret_print("KW_DEFFUN"); }
"for"    			{ ret_print("KW_FOR"); }
"if"    			{ ret_print("KW_IF"); }
"exit"    		{ ret_print("KW_EXIT"); }
"load"    		{ ret_print("KW_LOAD"); }
"disp"    		{ ret_print("KW_DISP"); }
"true"    		{ ret_print("KW_TRUE"); }
"false"   		{ ret_print("KW_FALSE"); }

"+"						{ ret_print("OP_PLUS"); }
"-"						{ ret_print("OP_MINUS"); }
"/"						{ ret_print("OP_DIV"); }
"*"						{ ret_print("OP_MULT"); }
"("						{ ret_print("OP_OP"); }
")"						{ ret_print("OP_CP"); }
"**"					{ ret_print("OP_DBLMULT"); }
"“"						{ ret_print("OP_OC"); }
"”"						{ ret_print("OP_CC"); }
","						{ ret_print("OP_COMMA"); }

{ID} 			{ ret_print("IDENTIFIER"); }
{ICONST} 		{ ret_print("VALUE"); }

"\n"			{  }
[ \t\r\f]+			/* eat up whitespace */

.				{ yyerror("SYNTAX_ERROR"); }

%%

void ret_print(char *token_type){
	printf("%s\t%s\n", yytext, token_type);
}

void yyerror(char *message){
	printf("%s %s cannot be tokenized\n", message, yytext);
	exit(1);
}

int main(int argc, char *argv[]){
	yyin = fopen(argv[1], "r");
	yylex();
	fclose(yyin);
	return 0;
}