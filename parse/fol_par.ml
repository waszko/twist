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

open Parsing;;
let _ = parse_error;;
# 2 "fol_par.mly"
open Expr (* contains expr type declarations *)
# 20 "fol_par.ml"
let yytransl_const = [|
  258 (* QUANTIFIER *);
  259 (* IN *);
  262 (* AND *);
  263 (* OR *);
  264 (* NOT *);
  265 (* EQUALS *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* SEMICOLON *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  260 (* LCHAR *);
  261 (* UCHAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\005\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\002\000\003\000\005\000\004\000\
\003\000\001\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\013\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\012\000\006\000\003\000\
\004\000\009\000\000\000\008\000\011\000\000\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\021\000\011\000"

let yysindex = "\014\000\
\004\255\000\000\015\255\000\000\019\255\004\255\004\255\000\000\
\011\255\000\000\021\255\028\255\029\255\254\254\252\254\004\255\
\004\255\000\000\029\255\027\255\009\255\000\000\000\000\000\000\
\000\000\000\000\004\255\000\000\000\000\254\254"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\255"

let yygindex = "\000\000\
\000\000\250\255\000\000\000\000\003\000"

let yytablesize = 33
let yytable = "\014\000\
\015\000\016\000\017\000\016\000\017\000\003\000\023\000\004\000\
\005\000\024\000\025\000\006\000\004\000\007\000\001\000\022\000\
\016\000\017\000\012\000\028\000\030\000\026\000\018\000\029\000\
\005\000\005\000\007\000\007\000\013\000\019\000\020\000\027\000\
\004\000"

let yycheck = "\006\000\
\007\000\006\001\007\001\006\001\007\001\002\001\011\001\004\001\
\005\001\016\000\017\000\008\001\004\001\010\001\001\000\013\000\
\006\001\007\001\004\001\011\001\027\000\019\000\012\001\021\000\
\011\001\012\001\011\001\012\001\010\001\009\001\003\001\005\001\
\004\001"

let yynames_const = "\
  QUANTIFIER\000\
  IN\000\
  AND\000\
  OR\000\
  NOT\000\
  EQUALS\000\
  LPAREN\000\
  RPAREN\000\
  SEMICOLON\000\
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
# 18 "fol_par.mly"
                                             (_1)
# 111 "fol_par.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_sentence) in
    Obj.repr(
# 22 "fol_par.mly"
                                             (_1)
# 118 "fol_par.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sentence) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 23 "fol_par.mly"
                                             (And(_1,_3))
# 126 "fol_par.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sentence) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 24 "fol_par.mly"
                                             (Or(_1,_3))
# 134 "fol_par.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 25 "fol_par.mly"
                                             (Not(_2))
# 141 "fol_par.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sentence) in
    Obj.repr(
# 26 "fol_par.mly"
                                             (_2)
# 148 "fol_par.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 27 "fol_par.mly"
                                             (Quant(_2,_4,_5))
# 157 "fol_par.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 32 "fol_par.mly"
                                             (Pred(_1,_3))
# 165 "fol_par.ml"
               : 'atomic_sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 33 "fol_par.mly"
                                             (Eq(_1,_3))
# 173 "fol_par.ml"
               : 'atomic_sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "fol_par.mly"
                                             (Var _1)
# 180 "fol_par.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 45 "fol_par.mly"
                                             (Terms(_1,_2))
# 188 "fol_par.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 46 "fol_par.mly"
                                             (Term(_1))
# 195 "fol_par.ml"
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
