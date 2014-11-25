%{
#include <stdio.h>
#include <string.h>

void yyerror(const char *str)
{
	fprintf(stderr,"error: %s\n",str);
}

int yywrap()
{
	return 1;
}

main()
{
	yyparse();
}

char *heater="default"; //var to store heater name used later

int yydebug=1; //enable debugging

%}

%token TOKHEATER TOKHEAT TOKTARGET TOKTEMPERATURE

%union
{
	int number;
	char *string;
}

%token <number> STATE
%token <number> NUMBER
%token <string> WORD

%%

commands: /* empty */
		| commands command
		;

command:
	    heat_switch
		|
		target_set
		|
		heater_select	
		;

heat_switch:
		TOKHEAT STATE
		{
			if($2)
				printf("\tHeat turned on\n");
			else
				printf("\tHeat turned off\n");
		}
		;

target_set:
		TOKTARGET TOKTEMPERATURE NUMBER
		{
			printf("\tHeater'%s' temperature set to %d\n",heater,$3);
		}
		;

heater_select:
		TOKHEATER WORD
		{
			printf("\tSelected heater '%s'\n",$2);
			heater=$2;
		}
		;

%%
