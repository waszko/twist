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

// hard coded graph from my book (14th Nov)
int vertices[4] = {1,2,3,4};
int edges[3][2] = {
	{1,3},
	{1,4},
	{3,4} 
};

%}

%token IN AND OR NOT '(' ')' '=' ','

%union
{
	int num;
	char chr;
	char *str;
}

%token <num> NUMBER
%token <chr> LCHAR 
%token <chr> UCHAR 
%token <str> QUANTIFIER 

%%

sentence
		: atomic_sentence
		| sentence '&' sentence
		| sentence '|' sentence
		| '~' sentence
		| '(' sentence ')'
		| QUANTIFIER variables IN UCHAR sentence /* was "quant VAR sent" */
		{
			char connector;
			if (!strcmp($1,"forall")) {
				connector = '&';
			} else {
				connector = '|';
			}
				printf("Connector to expand on is %c\n",connector);
			// $$ = for i 0 to UCHAR length {
			//		$5 with var replaced with UCHAR[i]) + connector
			// 		}
		}
		;

variables								/* added by me */
		:  LCHAR 
		| '(' variables ')'
		| variables ',' LCHAR
		;

atomic_sentence
		: UCHAR '(' term_list ')' /* this is a predicate */ 
		| term '=' term
		;

term
			/* this is a function - do i even want functions? */
		: LCHAR '(' term_list ')'
			/* variable */
		| LCHAR 				 
			/* constant */
		| NUMBER			 
		;

term_list
			/* comma? */
		: term_list term 
		| term 
		;

%%
