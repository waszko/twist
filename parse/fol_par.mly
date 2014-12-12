%{
open Expr (* contains expr type declarations *)
%}

%token <int> INT
%token QUANTIFIER IN 
%token <string> LCHAR UCHAR /* ? char ? */
%token AND OR NOT EQUALS
%token LPAREN RPAREN

%left AND OR                /* ?? */ 
%start main 
%type <Expr.expr> main           /* type? */ 

%%

main:
      sentence                               {$1}
;

sentence:
      atomic_sentence                        {$1}
    | sentence AND sentence                  {And($1,$3)}
    | sentence OR sentence                   {Or($1,$3)}
    | NOT sentence                           {Not($2)}
    | LPAREN sentence RPAREN                 {$2}
    | QUANTIFIER LCHAR IN UCHAR sentence     {Quant($2,$4,$5)}
;

atomic_sentence:
      /* predicate */
      UCHAR LPAREN term_list RPAREN          {Pred($1,$3)} 
    | term EQUALS term                       {Eq($1,$3)}
;

term:                    
      /* variable */
      LCHAR                                  {Var $1} /* parens? */  
      /* constant *?
    | INT                                    {Const $1}
      /* also functions? */
;

term_list:
      term_list term                         {Terms($1,$2)} /* bad */
    | term                                   {Term($1)}
;
