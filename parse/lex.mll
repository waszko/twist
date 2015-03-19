{
    open Parse 
}

rule token = parse
      [' ' '\t' '\n']      { token lexbuf }  (* skip whitespace *)
    | ['G''g']"iven"       { GIVEN }
    | ['F''f']"ind"        { FIND }
    | ['S''s']"atisfying"  { SATISFYING }
    | "forall"             { FORALL }
    | "exists"             { EXISTS }
    | "in"                 { IN }
    | "of"                 { OF }
    | ['a'-'z']+ as id     { LCHAR id } (* dunno about id thing *) 
    | ['A'-'Z']+ as id     { UCHAR id } (* ... also make these chars? *)
    | ['0'-'9']+ as lxm    { INT (int_of_string lxm) } 
    | '&'                  { AND }
    | '|'                  { OR }
    | '~'                  { NOT }
    | "->"                 { IMPLIES }
    | '='                  { EQUALS }
    | "<="                 { LEQ }
    | ">="                 { GEQ }
    | '('                  { LPAREN }
    | ')'                  { RPAREN }
    | ':'                  { COLON }
    | "(*"                 { comment lexbuf } (* start of comment *)
    | eof                  { EOF }

(* does not allow nested comments *)
and comment = parse
    | "*)"                 { token lexbuf } (* end of comment *)
    | _                    { comment lexbuf }

