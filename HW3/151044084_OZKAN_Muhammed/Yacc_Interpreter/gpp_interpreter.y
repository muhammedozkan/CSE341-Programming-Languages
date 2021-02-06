/*********************************
* CSE341 - Programming Languages *
*                                *
*    151044084 Muhammed ÖZKAN    *
*                                *
**********************************/

%{
	#include <stdio.h>
	#include <string.h>

	int getSTR(char *_str, char _listString[1000][200], int _numString)
	{
	int i;
	    for(i = 0; i < _numString; i++ )
	        if (strcmp(_str, _listString[i]) == 0 )
	            return i;

	    return -1;
	}

	void capitalSTR(char _string[])
	{
   	int temp = 0;
   
   	while (_string[temp] != '\0') {
      	if (_string[temp] >= 'a' && _string[temp] <= 'z') {
         	_string[temp] = _string[temp] - 32;
      	}
      	temp++;
   	}
	}

	void copy(int _v1[], int _v2[], int _size)
	{
	 
	for(int i = 0; i < _size && i < 999; i++)
		_v1[i] = _v2[i];
	}

	struct mystruct
	{ 
	   char identifier[1000][200]; 
	   int values[1000]; 
	   int count;
	};

	struct mystruct identifiers = {"", 0, 0};

	
	int var1 = 0;
	int var2 = 0;
	int sum = 0;

%}


%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_DEFVAR KW_FOR KW_IF KW_EXIT KW_LOAD
%token KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV OP_DBLMULT OP_MULT OP_OP OP_CP OP_OC OP_CC OP_COMMA COMMENT IDENTIFIER VALUE CUT FILENAME
%start START

%union 
{
	struct 
	{
	int cond;
	int value;
	char *string;
	int values[1000];
       };
}
%%

START: | START INPUT { var1 = 0; var2 = 0; sum = 0;};

INPUT:
	EXPI{
	if($<cond>$ == 0)
	{printf("%d\n", $<value>1); $<value>$ = $<value>1; $<cond>$ = 0;}
	else if($<cond>$ == 1)
	{printf("%s\n", $<string>1); $<string>$ = $<string>1; $<cond>$ = 1;}
	else if($<cond>$ == 2)
	{
		if($<value>$ == 1) printf("TRUE\n");
		else printf("FALSE\n");
		$<value>$ = $<value>1;
		$<cond>$ = 2;
	}
	else if($<cond>$ == 3)
	{
		int j;
		printf("(");
		for(j = 0; j < var2; j += 1)
    		{printf("%d", $<values>1[j]);
    		if(j != var2-1) printf(" ");}
    	printf(")\n");
    	copy($<values>$, $<values>1, var2);
		$<cond>$ = 3;
	}
		}
	| EXPLISTI {
	if($<cond>$ == 0)
	{printf("%d\n", $<value>1); $<value>$ = $<value>1; $<cond>$ = 0;}
	else if($<cond>$ == 1)
	{printf("%s\n", $<string>1); $<string>$ = $<string>1; $<cond>$ = 1;}
	else if($<cond>$ == 2)
	{
		if($<value>$ == 1) printf("T\n");
		else printf("NIL\n");
		$<value>$ = $<value>1;
		$<cond>$ = 2;
	}
	else if($<cond>$ == 3)
	{
		int j;
		printf("(");
		for(j = 0; j < var2; j += 1)
            		{printf("%d", $<values>1[j]);
            		if(j != var2-1) printf(" ");}
            	printf(")\n");
            	copy($<values>$, $<values>1, var2);
		$<cond>$ = 3;
	}
		}
	| COMMENT {$<cond>$ = 5;};

LISTVALUE:
	OP_OP KW_LIST VALUES OP_CP {
	copy($<values>$, $<values>3, var1);
	$<cond>$ = 3;
		}
	| CUT OP_OP VALUES OP_CP {
	copy($<values>$, $<values>3, var1);
	$<cond>$ = 3;
	 	};

VALUES:
	VALUES VALUE{
	$<values>$[var1] = $<value>2;
	var1 += 1;
	$<cond>$ = 3;
		 }
	| VALUES IDENTIFIER{
		capitalSTR($<string>2);
		int res = getSTR($<string>2, identifiers.identifier, identifiers.count);
		if(res != -1)
		{
	$<value>2 = identifiers.values[res];
	$<cond>$ = 0;
		}
		else
		{
	printf(" variable %s has no value\n", $<string>2);
	exit(0);
		}
		$<values>$[var1] = $<value>2;
		var1 += 1;
		$<cond>$ = 3;
	 }
	| VALUE {$<values>$[var1] = $<value>1; var1 += 1; $<cond>$ = 3;};

IDENTIFIERS:
	IDENTIFIERS IDENTIFIER{
	capitalSTR($<string>1);
	$<string>$ = $<string>1;
	strcat($<string>$, " ");
	strcat($<string>$, $<string>2);
	$<cond>$ = 1;
		  }
	| IDENTIFIER {
	capitalSTR($<string>1);
	int res = getSTR($<string>1, identifiers.identifier, identifiers.count);
	if(res == -1)
	{
		strcpy(identifiers.identifier[identifiers.count], $<string>1);
		identifiers.values[identifiers.count] = -1;
		identifiers.count += 1;
	}
	$<string>$ = $<string>1; $<cond>$ = 1;
		 };

EXPI:
	OP_OP OP_PLUS EXPI EXPI OP_CP {$<value>$ = $<value>3 + $<value>4; $<cond>$ = 0;}
	| OP_OP OP_MINUS EXPI EXPI OP_CP {$<value>$ = $<value>3 - $<value>4; $<cond>$ = 0;}
	| OP_OP OP_MULT EXPI EXPI OP_CP {$<value>$ = $<value>3 * $<value>4; $<cond>$ = 0;}
	| OP_OP OP_DIV EXPI EXPI OP_CP {
	if($<value>4 == 0)
		{printf("Can not divide by zero\n");
		exit(0);}
	else
		{$<value>$ = $<value>3 / $<value>4;
		$<cond>$ = 0;}
	}
	| OP_OP OP_DBLMULT EXPI EXPI OP_CP {
		int x = $<value>3;
		int j;
		for(j = 1;j<$<value>4;j++)
	x *=$<value>3;
		$<value>$ = x;
		$<cond>$ = 0;
	   }
	| OP_OP IDENTIFIER EXPLISTI OP_CP {capitalSTR($<string>2); $<string>$ = $<string>2; $<cond>$ = 1;}
	| OP_OP IDENTIFIER EXPI OP_CP {capitalSTR($<string>2); $<string>$ = $<string>2; $<cond>$ = 1;}
	| OP_OP KW_SET IDENTIFIER EXPI OP_CP{
		capitalSTR($<string>3);
		int res = getSTR($<string>3, identifiers.identifier, identifiers.count);
		if(res == -1)
		{
	strcpy(identifiers.identifier[identifiers.count], $<string>3);
	identifiers.values[identifiers.count] = $<value>4;
	identifiers.count += 1;
		}
		else
	{identifiers.values[res] = $<value>4;}

		$<value>$ = $<value>4;
		$<cond>$ = 0;
		}
	| OP_OP KW_SET IDENTIFIER EXPLISTI OP_CP{
	copy($<values>$, $<values>4, var2);
	$<cond>$ = 3;
	}
	| OP_OP KW_IF EXPB EXPLISTI OP_CP{
		if($<value>3 == 1)
		{
	copy($<values>$, $<values>4, var2);
	$<cond>$ = 3;
		}
		else
		{
	$<value>$ = 0;
	$<cond>$ = 3;
		}
	 }
	| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP{
	if($<value>3 == 1)
	{
	copy($<values>$, $<values>4, var2);
	$<cond>$ = 3;
	}
	else
	{
	copy($<values>$, $<values>5, var2);
	$<cond>$ = 3;
	}
	 }
	| OP_OP KW_IF EXPB EXPI OP_CP{
	if($<value>3 == 1)
	{
		$<value>$ = $<value>4;
		$<cond>$ = 0;
	}
	else
	{
		$<value>$ = 0;
		$<cond>$ = 3;
	}
		 }
	| OP_OP KW_IF EXPB EXPI EXPI OP_CP{
		if($<value>3 == 1)
		{
	$<value>$ = $<value>4;;
	$<cond>$ = 0;
		}
		else
		{
	$<value>$ = $<value>5;
	$<cond>$ = 0;
		}
	 }
	| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP {
	copy($<values>$, $<values>8, var2);
	$<cond>$ = 3;
		 }
	| OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP{
	capitalSTR($<string>3);
	strcpy(identifiers.identifier[identifiers.count], $<string>3);
	identifiers.values[identifiers.count] = $<value>4;
	identifiers.count += 1;
	$<string>$ = $<string>3;
	$<cond>$ = 1;
	  	}
	| OP_OP KW_DEFVAR IDENTIFIER EXPLISTI OP_CP{
	capitalSTR($<string>3);
	$<string>$ = $<string>3;
	$<cond>$ = 1;
	  	}
	| OP_OP KW_LOAD OP_OC FILENAME OP_CC OP_CP {
	FILE* fp = fopen($<string>4, "r");
	$<value>$ = 0;
	$<cond>$ = 2;
	if(fp == NULL) printf("A file with name %s does not exist\n", $<string>4);
		    	else{
		    		printf(";; Loaded file %s\n", $<string>4);
		    		$<value>$ = 1;
		    	}
	   }
	| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPI OP_CP {
	capitalSTR($<string>3);
	strcpy(identifiers.identifier[identifiers.count], $<string>3);
	identifiers.values[identifiers.count] = -1;
	identifiers.count += 1;
	$<string>$ = $<string>3;
	$<cond>$ = 1;
	}
	| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPLISTI OP_CP {
		capitalSTR($<string>3);
		strcpy(identifiers.identifier[identifiers.count], $<string>3);
		identifiers.values[identifiers.count] = -1;
		identifiers.count += 1;
		$<string>$ = $<string>3;
		$<cond>$ = 1;
		}
	| OP_OP KW_DEFFUN IDENTIFIER EXPLISTI OP_CP {
	capitalSTR($<string>3);
	strcpy(identifiers.identifier[identifiers.count], $<string>3);
	identifiers.values[identifiers.count] = -1;
	identifiers.count += 1;
	$<string>$ = $<string>3;
	$<cond>$ = 1;
	}
	| OP_OP KW_EXIT OP_CP {printf("Exiting.\n"); exit(1);}
	| OP_OP KW_DISP EXPI OP_CP {
	if($<cond>3 == 0)
	{$<value>$ = $<value>3; $<cond>$ = 0;}
	else if($<cond>3 == 1)
	{$<string>$ = $<string>3; $<cond>$ = 1;}
	else if($<cond>3 == 2)
	{$<value>$ = $<value>3; $<cond>$ = 2;}
	else if($<cond>3 == 3)
	{copy($<values>$, $<values>3, var2); $<cond>$ = 3;}
		}
	| OP_OP KW_DISP EXPLISTI OP_CP {
	copy($<values>$, $<values>3, var2); $<cond>$ = 3;
		   }
	| EXPB {$<value>$ = $<value>1; $<cond>$ = $<cond>1;};

EXPB:
	OP_OP KW_AND EXPB EXPB OP_CP {$<value>$ = $<value>3 && $<value>4; $<cond>$ = 2;}
	| OP_OP KW_OR EXPB EXPB OP_CP {$<value>$ = $<value>3 || $<value>4; $<cond>$ = 2;}
	| OP_OP KW_NOT EXPB OP_CP {if($<value>3 == 0) $<value>$ = 1; else $<value>$ = 0; $<cond>$ = 2;}
	| OP_OP KW_EQUAL EXPB EXPB OP_CP {if($<value>3 == $<value>4) $<value>$ = 1; else $<value>$ = 0; $<cond>$ = 2;}
	| OP_OP KW_LESS EXPB EXPB OP_CP {if($<value>3 < $<value>4) $<value>$ = 1; else $<value>$ = 0; $<cond>$ = 2;}
	| VALUE {$<value>$ = $<value>1; $<cond>$ = 0;}	
	| KW_TRUE {$<value>$ = 1; $<cond>$ = 2;}
	| KW_FALSE {$<value>$ = 0; $<cond>$ = 2;}
	| KW_NIL {$<value>$ = 0; $<cond>$ = 2;}
	| IDENTIFIER {
	capitalSTR($<string>1);
	int res = getSTR($<string>1, identifiers.identifier, identifiers.count);
	if(res != -1)
	{
		$<value>$ = identifiers.values[res];
		$<cond>$ = 0;
	}
	else
	{
		printf(" variable %s has no value\n", $<string>1);
		exit(0);
	}
		 };

EXPLISTI:
	OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {
	int j, x = 0;
	
for(j = sum - var2; j < sum; j++, x++)
	{$<values>3[j] = $<values>4[x];}
	copy($<values>$, $<values>3, sum);
	$<cond>$ = 3;
	var2 = sum;
	 }
	| OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP {
	int j, x = 0;
	for(j = sum - var2; j < sum; j++, x++)
	{$<values>3[j] = $<values>4[x];}
	copy($<values>$, $<values>3, sum);
	$<cond>$ = 3;
	var2 = sum;
	  }
	| OP_OP KW_APPEND EXPI EXPLISTI OP_CP {
	int j;
	for(j = 0; j < var2; j++)
	{$<values>$[j+1] = $<values>4[j];}
	$<values>$[0] = $<value>3;
	$<cond>$ = 3;
	var2 += 1;
		  }
	| LISTVALUE {
	copy($<values>$, $<values>1, var1);
	$<cond>$ = 3;
	var2 = var1;
	sum += var1;
	var1 = 0;
		};

%%



