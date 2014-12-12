%{
#include <stdio.h>
#include "y.tab.h"
%}

%%
for[ ]?all|exists	yylval.str=strdup(yytext); return QUANTIFIER;
in					return IN;
	/* do we want tokens for logic symbols? */
[&\|~]				return *yytext;
[0-9]+				yylval.str=strdup(yytext); return NUMBER;
	/* ^ currently passing numbers as strings */
[a-z]				yylval.str=strdup(yytext); return LCHAR;
[A-Z]				yylval.str=strdup(yytext); return UCHAR;
 	/* ^ is only chars ok? - and passing them as strings? */
	/* do we want tokens for operators? */
[=,\(\)]			return *yytext;
\n					/* ignore EOL */;	
[ \t]+				/* ignore whitespace */;
%%
