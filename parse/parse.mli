type token =
  | INT of (int)
  | FORALL
  | EXISTS
  | IN
  | OF
  | LCHAR of (string)
  | UCHAR of (string)
  | AND
  | OR
  | NOT
  | IMPLIES
  | EQUALS
  | LPAREN
  | RPAREN
  | SEMICOLON
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
