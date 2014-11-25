%{
#include <stdio.h>
#include "y.tab.h"
%}

%%
forall				return FORALL;
exists				return EXISTS;
in					return IN;
[g|G]iven			return GIVEN;
[f|F]ind			return FIND;
[s|S]atisfying		return SATISFYING;
\*					return AND;
\+					return OR;
~					return NOT;
[0-9]+				yylval.num=atoi(yytext); return NUMBER;
[a-zA-Z]+			yylval.str=strdup(yytext); return NAME; /*dup=bad*/
					/* ^ do we want to only allow chars here? */
\(					return OPAREN;
\)					return CPAREN;	
=					return EQUALS;
:					return COLON;	
,					return COMMA; /* needed? */
\n					/* ignore EOL */;	
[ \t]+				/* ignore whitespace */;
%%
