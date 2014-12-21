(* write dimacs_cnf string to "out.cnf" file *)
let write_cnf str =
    let oc = open_out "out.cnf" in
    Printf.fprintf oc "%s" str;
    close_out oc

(* call minisat on "out.cnf", storing result in "out.txt" *)
let call_minisat () =
    print_string( "Minisat exit code: " ^ string_of_int (
        Sys.command "../sat_solvers/minisat/minisat out.cnf out.txt" ) );
    print_newline();
    flush stdout

let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Parse.main Lex.token lexbuf in
            let expanded = Expand.expand_expr result in
            print_string ( Expr.string_of_expr expanded );
            print_newline(); print_newline();
            let (subbed, nbvars) = Sub.sub_expr_call expanded in
            let cnf = Cnf.cnf_expr subbed in
            print_newline(); flush stdout;
            let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
            write_cnf dimacs;
            
            call_minisat ();
        done
    with Lex.Eof ->
        exit 0
