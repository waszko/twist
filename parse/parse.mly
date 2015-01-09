%{
open Expr (* contains expr type declarations *)
%}

%token <int> INT
%token FORALL EXISTS IN 
%token <string> LCHAR UCHAR /* ? char ? */
%token AND OR NOT IMPLIES EQUALS
%token LPAREN RPAREN SEMICOLON EOF

%left AND OR                /* ?? */ 
%start main 
%type <Expr.expr> main           /* type? */ 

%%

main:
      sentence EOF                           {$1} /* semicolon? */
;

sentence:
      atomic_sentence                        {$1}
    | sentence AND sentence                  {And($1,$3)}
    | sentence OR sentence                   {Or($1,$3)}
    | NOT sentence                           {Not($2)}
    | sentence IMPLIES sentence              {Or(Not($1),$3)}
    | LPAREN sentence RPAREN                 {$2}
    | FORALL term_list IN UCHAR sentence {Forall(Terms(List.rev $2),$4,$5)}
    | EXISTS term_list IN UCHAR sentence {Exists(Terms(List.rev $2),$4,$5)}
;

atomic_sentence:
      UCHAR LPAREN term_list RPAREN          {Pred($1,Terms(List.rev $3))} 
    | term EQUALS term                       {Eq($1,$3)}
;

term:                    
      LCHAR                                  {Var $1} /* parens? */  
    | INT                                    {Const $1}
      /* also functions? */
;

term_list:
      term_list term                         {$2::$1} /* list */
    | term                                   {[$1]}
;
