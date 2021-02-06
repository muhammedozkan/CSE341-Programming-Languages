/*********************************
* CSE341 - Programming Languages *
*                                *
*    151044084 Muhammed ÖZKAN    *
*                                *
**********************************/


%option noyywrap
%{ 
#include <stdio.h>
#include "y.tab.h"
int count = 0;
%} 

blank [ \t\n]+

%%
[aA][nN][dD][ ]					return KW_AND;
[oO][rR][ ]						return KW_OR;
[nN][oO][tT][ ]					return KW_NOT;
[eE][qQ][uU][aA][lL][ ] 				return KW_EQUAL;
[lL][eE][sS][sS][ ]					return KW_LESS;
[nN][iI][lL][ ]					return KW_NIL;
[lL][iI][sS][tT][ ]					return KW_LIST;
[aA][pP][pP][eE][nN][dD][ ]				return KW_APPEND;
[cC][oO][nN][cC][aA][tT][ ]				return KW_CONCAT;
[sS][eE][tT][ ]					return KW_SET;
[dD][eE][fF][fF][uU][nN][ ]				return KW_DEFFUN;
[dD][eE][fF][vV][aA][rR][ ]				return KW_DEFVAR;
[fF][oO][rR][ ]					return KW_FOR;
[iI][fF][ ]						return KW_IF;
[eE][xX][iI][tT]					return KW_EXIT;
[lL][oO][aA][dD][ ]					return KW_LOAD;
[dD][iI][sS][pP][ ]					return KW_DISP;
[tT][rR][uU][eE]					return KW_TRUE;
[fF][aA][lL][sS][eE]					return KW_FALSE;

[+][ ] 						return OP_PLUS;
[-][ ] 						return OP_MINUS;
[/][ ] 						return OP_DIV;
[*][*][ ] 						return OP_DBLMULT;
[*][ ] 						return OP_MULT;
[(] 							return OP_OP;
[)] 							return OP_CP;
["] 						{
							count += 1;
							if(count % 2 == 1) return OP_OC;
							else return OP_CC;
						}

[,]							return OP_COMMA;

[;][;][ a-zA-Z0-9\/.+-]+			return COMMENT;

[a-zA-Z_][a-zA-Z0-9_]*				{
							yylval.string=strdup(yytext);
							return IDENTIFIER;
						}

[0]|[1-9][0-9]*				{	
							yylval.value=atoi(yytext);
							return VALUE;
						}
[']							return CUT;
[a-zA-Z0-9\/.+-]+				{
							yylval.string=strdup(yytext);
							return FILENAME;
						}


{blank} 					{;}
.						{yyerror("SYNTAX_ERROR\n");}

%%

int yyerror(char *msg) 
{ 
  printf("SYNTAX_ERROR Expression not recognized\n");
  exit(0); 
}

int main(int argc, char *argv[]){
	--argc;
	++argv;
	
	if(argc > 0){ 
		yyin = fopen(argv[0], "r");
		yyparse();
		fclose(yyin);
		
	}
	else{	
			
		char input[500];
	
     		do{
			printf(">> ");
	 		fgets (input, 500, stdin);
    	 		if(strlen(input)==1)
    				break;
    	
			YY_BUFFER_STATE bufferState = yy_scan_string(input);
      
    			int token;
    			do {
       			 token = yyparse();
    			} while (token != 0);

 		
  			 yy_delete_buffer(bufferState);
   
	
		}while(1);
      

   	 return (EXIT_SUCCESS);
		
		

	}

	return 0;
}
