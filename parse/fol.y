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

%token IN AND OR NOT OPAREN CPAREN COMMA EQUALS

%union
{
	int num;
	char *str;
}

%token <num> NUMBER
%token <str> NAME
%token <str> QUANTIFIER 

%%

sentence
		: atomic_sentence
		| sentence AND sentence
		| sentence OR sentence
		| NOT sentence
		| OPAREN sentence CPAREN
		| QUANTIFIER variables IN NAME sentence /* was "quant VAR sent" */
		{
			char connector;
			if (!strcmp($1,"forall")) {
				printf("FOR ALL\n");
				connector = '&';
			} else {
				printf("EXISTS\n");
				connector = '|';
			}
				printf("Connector to expand on is %c\n",connector);
			// $$ = for i 0 to NAME length {
			//		$5 with var replaced with NAME[i]) + connector
			// 		}
		}
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

%%
