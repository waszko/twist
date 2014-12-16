let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Parse.main Lex.token lexbuf in
                print_string (Expr.string_of_expr (
                        Expand.expand_expr result )); 
                print_newline(); flush stdout
        done
    with Lex.Eof ->
        exit 0
