type token =
  | INT of (int)
  | QUANTIFIER
  | IN
  | LCHAR of (string)
  | UCHAR of (string)
  | AND
  | OR
  | NOT
  | EQUALS
  | LPAREN
  | RPAREN
  | SEMICOLON

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
