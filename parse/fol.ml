let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Fol_par.main Fol_lex.token lexbuf in
                print_string (Expr.string_of_expr result); 
                print_newline(); flush stdout
        done
    with Fol_lex.Eof ->
        exit 0
