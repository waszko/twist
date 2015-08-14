%{
open Expr (* contains expr type declarations *)
%}

%token <int> INT
%token GIVEN FIND SATISFYING FORALL EXISTS IN OF
%token <string> LCHAR UCHAR /* ? char ? */
%token AND OR NOT IMPLIES EQUALS LEQ GEQ
%token LPAREN RPAREN COLON EOF

%right IMPLIES
%left OR
%left AND 
%right NOT

%start main 
%type <(string list * Expr.expr)> main           /* type? */ 

%%

main:
      preamble SATISFYING COLON sentence EOF {($1,$4)}
;

preamble:
      GIVEN COLON pred_list FIND COLON pred_list {$3}
        /* only currently using 'given' list */

sentence:
      atomic_sentence                        {$1}
    | sentence AND sentence                  {And($1,$3)}
    | sentence OR sentence                   {Or($1,$3)}
    | NOT sentence                           {Not($2)}
    | sentence IMPLIES sentence              {Or(Not($1),$3)}
    | LPAREN sentence RPAREN                 {$2}
    | FORALL term_list IN UCHAR sentence     {Forall(Terms(List.rev $2),
                                              $4,$5)}
    | EXISTS term_list IN UCHAR sentence     {Exists(Terms(List.rev $2),
                                              $4,$5)}
    | OR UCHAR OF UCHAR OR eq UCHAR          {Card($2,$4,$6,$7)}
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

pred_list:
      pred_list UCHAR                        {$2::$1} /* list */
    | UCHAR                                  {[$1]}
;

eq:
      EQUALS                                 {"="} /* get from tokens? */
    | LEQ                                    {"<="}
    | GEQ                                    {">="}
