%{
#include <stdio.h>
#include <string.h>

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
	yyparse();
}

%}

%token FORALL EXISTS IN AND OR NOT OPAREN CPAREN COLON COMMA GIVEN FIND SATISFYING

%union
{
	int num;
	char *str;
}

%token <num> NUMBER
%token <str> NAME

%%

problem:
		givenDesc findDesc satDesc
		;

givenDesc:
		GIVEN COLON givens
		;

givens: 
	  	/* empty - should these be at least 1? */ 
		| givens given 
		;

given:
		NAME OPAREN NUMBER CPAREN
		{
			printf("Given %s of order %d",$1,$3);
		} 
		;
		
findDesc:
		FIND COLON finds
		;

finds: 	
	 	/* empty */
		| finds find 
		;

find:
		NAME OPAREN NUMBER CPAREN
		{
			printf("Find %s of order %d",$1,$3);
		} 
		;
		
satDesc:
		SATISFYING COLON formula
		;

formula:
		forall disjunction
		| exists disjunction
		| disjunction
		;

disjunction:
		conjunction
		| disjunction OR conjunction
		;

conjunction:
		negation
		| conjucntion AND negation
		;

negation:
		NOT predicate
		| NOT OPAREN formula CPAREN
		| OPAREN formula CPAREN
		;

predicate

exprs:
		expr
		| forallExpr
		| existsExpr
		| exprs connector expr
		;

expr:
		NOT expr
		| OPAREN exprs CPAREN
		;

connector:
		 AND
		| OR
		;

forallExpr:
		FORALL bound IN NAME OPAREN expr CPAREN
		;

existsExpr:
		EXISTS bound IN NAME OPAREN expr CPAREN
		;
		
bound:
		NAME
		| bound COMMA NAME
		;

%%
