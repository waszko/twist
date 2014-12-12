/* from www.tldp.org/HOWTO/Lex-YACC-HOWTO-1.html */

%{
#include <stdio.h>
%}

%%
stop  					printf("Stop received ");
start 					printf("Start received ");
[0-9]+					printf("NUMBER ");
[a-zA-Z][a-zA-Z0-9]*	printf("WORD ");
[a-zA-Z0-9\/.-]+		printf("FILENAME ");
\"						printf("QUOTE ");
\{						printf("OBRACE ");
\}						printf("EBRACE ");
;						printf("SEMICOLON ");
\n						printf("\n");
[ \t]+					/* ignore whitespace */;
%%
