%{
#include <stdio.h>
#include "y.tab.h"
%}

%%
forall|exists		yylval.str=strdup(yytext); return QUANTIFIER;
in					return IN;
	/* do we want tokens for logic symbols? */
[&\|~]				return *yytext;
[0-9]+				yylval.num=atoi(yytext); return NUMBER;
[a-z]				yylval.chr=yytext[0]; return LCHAR;
[A-Z]				yylval.chr=yytext[0]; return UCHAR;
 	/* ^ is only chars ok? */
	/* do we want tokens for operators? */
[=,\(\)]			return *yytext;
\n					/* ignore EOL */;	
[ \t]+				/* ignore whitespace */;
%%
