%{
#include <stdio.h>
#include <string.h>

extern FILE *yyin;

void yyerror(const char *str)
{
	fprintf(stderr,"error: %s\n",str);
}

int yywrap()
{
	return 1; // parse instance next? (set to 0?)
}

main()
{
	int readFromFile = 0;
	char * inFile = "3col.txt";
	if (readFromFile) {
		yyin = fopen(inFile,"r");
		if (yyin==NULL) {
			printf("\n Error reading file\n");
		}
	}
	yyparse();
	
}

%}

%token FORALL EXISTS IN AND OR NOT OPAREN CPAREN COMMA EQUALS

%union
{
	int num;
	char *str;
}

%token <num> NUMBER
%token <str> NAME

%%

sentence
		: atomic_sentence
		| sentence AND sentence
		| sentence OR sentence
		| quantifier variables IN NAME sentence /* was "quant VAR sent" */
		| NOT sentence
		| OPAREN sentence CPAREN
		;

variables								/* added by me */
		:  NAME
		| OPAREN variables CPAREN
		| variables COMMA NAME
		;

atomic_sentence
		: NAME OPAREN term_list CPAREN /* this name is predicate */ 
		| term EQUALS term
		;

term
		: NAME OPAREN term_list CPAREN /* this name is function */
		| NAME /* variable? */
		| NUMBER /* constant? */
		;

term_list
		: term_list term /* comma ? */
		| term
		;

quantifier
		: EXISTS
		| FORALL
		;

%%
