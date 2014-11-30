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

// relation?
typedef struct {
	char id;
	int length;
	int *values;
} rel;

// hard coded graph from my book (14th Nov)

int vertices[4] = {1,2,3,4};
int edges[3][2] = {
	{1,3},
	{1,4},
	{3,4} 
};

/*
rel v = {.id = 'V',
		 .length = 4,
		 .values = vertices
		};
*/

%}

%token IN AND OR NOT '(' ')' '=' ','

%union
{
	int num; // not currently used as numbers are passed as strings
	char chr;
	char *str; // not currently used as U/LCHARs passed as strings
}

%token <str> NUMBER
%token <str> LCHAR 
%token <str> UCHAR 
%token <str> QUANTIFIER 

%type <str> sentence atomic_sentence term term_list

%%

sentence
		: atomic_sentence		{ $$ = $1; }
		| sentence '&' sentence	{ $$ = strcat(strcat($1,"&"),$3); }
		| sentence '|' sentence { $$ = strcat(strcat($1,"|"),$3); }
		| '~' sentence			{ $$ = strcat("~",$2); }
		| '(' sentence ')'		{ $$ = strcat(strcat("(",$2),")"); }
		| QUANTIFIER LCHAR IN UCHAR sentence /* was "quant VAR sent" */
		{
			char connector;
			if (!strcmp($1,"forall")) {
				connector = '&';
			} else {
				connector = '|';
			}
				printf("Connector to expand on is %c\n",connector);
			// $$ = for i 0 to UCHAR length {
			//		$5 with LCHAR replaced with UCHAR[i]) + connector
			// 		}
		} 
		; 
/* add "variables" non-terminal : LCHAR | (vars) | vars LCHAR ? */

atomic_sentence
			/* this is a predicate */
		: UCHAR '(' term_list ')' 	{ $$ = strcat(
										     strcat(strcat($1,"("),$3),
										   ")");}
		| term '=' term 			{ $$ = strcat(strcat($1,"="),$3); }
		;

term
			/* this is a function - do i even want functions? */
		/*: LCHAR '(' term_list ')' */
			/* variable */
		: LCHAR 					{ $$ = $1; }	 
			/* constant */
		| NUMBER					{ $$ = $1; } 		
		;

term_list
			/* comma? */
		: term_list term 			{ $$ = strcat($1,$2); } /* spaces? */
		| term						{ $$ = $1; } 
		;

%%
