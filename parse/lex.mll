{
    open Parse 
    exception Eof
}

rule token = parse
      [' ' '\t' '\n']      { token lexbuf }  (* skip whitespace *)
    | ['0'-'9']+ as lxm    { INT (int_of_string lxm) } 
    | "forall"             { FORALL }
    | "exists"             { EXISTS }
    | "in"                 { IN }
    | ['a'-'z']+ as id     { LCHAR id } (* dunno about id thing *) 
    | ['A'-'Z']+ as id     { UCHAR id } (* ... also make these chars? *)
    | '&'                  { AND }
    | '|'                  { OR }
    | '~'                  { NOT }
    | '='                  { EQUALS }
    | '('                  { LPAREN }
    | ')'                  { RPAREN }
    | ';'                  { SEMICOLON }
    | eof                  { raise Eof }

