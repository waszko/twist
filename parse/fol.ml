let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Parse.main Lex.token lexbuf in
                let expanded = Expand.expand_expr result in
                  print_string ( Expr.string_of_expr expanded );
                  print_newline(); print_newline();
                  let cnf = Cnf.cnf_expr (Sub.sub_expr_call expanded) in
                   (* print_string ( Expr.string_of_expr cnf );
                    print_newline(); print_newline(); *)
                    print_string ( Dimacs.dimacs_of_expr cnf );
             (*   print_string ( Dimacs.dimacs_of_expr (
                                 Cnf.cnf_expr (
                                   Expand.expand_expr result
                              ) ) ); *)
                print_newline(); flush stdout
        done
    with Lex.Eof ->
        exit 0
