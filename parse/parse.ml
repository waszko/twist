type token =
  | INT of (int)
  | FORALL
  | EXISTS
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
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parse.mly"
open Expr (* contains expr type declarations *)
# 22 "parse.ml"
let yytransl_const = [|
  258 (* FORALL *);
  259 (* EXISTS *);
  260 (* IN *);
  263 (* AND *);
  264 (* OR *);
  265 (* NOT *);
  266 (* EQUALS *);
  267 (* LPAREN *);
  268 (* RPAREN *);
  269 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  261 (* LCHAR *);
  262 (* UCHAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\005\000\005\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\002\000\003\000\005\000\005\000\
\004\000\003\000\001\000\001\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\000\000\000\000\011\000\000\000\000\000\
\000\000\015\000\000\000\002\000\000\000\000\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\013\000\000\000\000\000\006\000\003\000\004\000\010\000\000\000\
\000\000\009\000\000\000\000\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\014\000\013\000"

let yysindex = "\012\000\
\032\255\000\000\000\000\041\255\041\255\000\000\012\255\032\255\
\032\255\000\000\004\000\000\000\014\255\010\255\000\000\035\255\
\041\255\000\255\037\255\032\255\032\255\000\000\041\255\019\255\
\000\000\020\255\015\255\000\000\000\000\000\000\000\000\032\255\
\032\255\000\000\000\255\000\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\003\000"

let yygindex = "\000\000\
\000\000\253\255\000\000\251\255\005\000"

let yytablesize = 271
let yytable = "\016\000\
\005\000\007\000\008\000\022\000\018\000\019\000\020\000\021\000\
\015\000\015\000\003\000\027\000\001\000\024\000\006\000\003\000\
\029\000\030\000\025\000\006\000\025\000\015\000\017\000\023\000\
\032\000\033\000\034\000\031\000\035\000\036\000\000\000\025\000\
\003\000\004\000\005\000\003\000\006\000\007\000\026\000\006\000\
\008\000\003\000\009\000\020\000\021\000\006\000\000\000\000\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\021\000\005\000\007\000\008\000"

let yycheck = "\005\000\
\000\000\000\000\000\000\000\000\008\000\009\000\007\001\008\001\
\004\000\005\000\001\001\017\000\001\000\004\001\005\001\001\001\
\020\000\021\000\014\000\005\001\016\000\017\000\011\001\010\001\
\006\001\006\001\012\001\023\000\032\000\033\000\255\255\027\000\
\001\001\002\001\003\001\001\001\005\001\006\001\004\001\005\001\
\009\001\001\001\011\001\007\001\008\001\005\001\255\255\255\255\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\007\001\008\001\012\001\012\001\012\001"

let yynames_const = "\
  FORALL\000\
  EXISTS\000\
  IN\000\
  AND\000\
  OR\000\
  NOT\000\
  EQUALS\000\
  LPAREN\000\
  RPAREN\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  LCHAR\000\
  UCHAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sentence) in
    Obj.repr(
# 18 "parse.mly"
                                             (_1)
# 178 "parse.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_sentence) in
    Obj.repr(
# 22 "parse.mly"
                                             (_1)
# 185 "parse.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sentence) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 23 "parse.mly"
                                             (And(_1,_3))
# 193 "parse.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sentence) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 24 "parse.mly"
                                             (Or(_1,_3))
# 201 "parse.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 25 "parse.mly"
                                             (Not(_2))
# 208 "parse.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sentence) in
    Obj.repr(
# 26 "parse.mly"
                                             (_2)
# 215 "parse.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 27 "parse.mly"
                                         (Forall(Terms(List.rev _2),_4,_5))
# 224 "parse.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 28 "parse.mly"
                                         (Exists(Terms(List.rev _2),_4,_5))
# 233 "parse.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 32 "parse.mly"
                                             (Pred(_1,Terms(List.rev _3)))
# 241 "parse.ml"
               : 'atomic_sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 33 "parse.mly"
                                             (Eq(_1,_3))
# 249 "parse.ml"
               : 'atomic_sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parse.mly"
                                             (Var _1)
# 256 "parse.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "parse.mly"
                                             (Const _1)
# 263 "parse.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "parse.mly"
                                             (_2::_1)
# 271 "parse.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "parse.mly"
                                             ([_1])
# 278 "parse.ml"
               : 'term_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.expr)
