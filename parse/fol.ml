(* call minisat on "out.cnf", storing result in "out.txt" *)
let call_minisat () =
    print_string( "Minisat exit code: " ^ string_of_int (
        Sys.command "../sat_solvers/minisat/minisat out.cnf out.txt" ) );
    print_newline();
    flush stdout

let _ =
    let problem = open_in "3col.txt" in
    let lexbuf = Lexing.from_channel problem in (* was stdin *)
    let instance = Io.read_instance () in
    let result = Parse.main Lex.token lexbuf in
    close_in problem;
    let expanded = Expand.expand_expr result instance in
    print_string ( Expr.string_of_expr expanded );
    print_newline(); print_newline();
    let (subbed, nbvars) = Sub.sub_expr_call expanded in
    let cnf = Cnf.cnf_expr subbed in
    print_newline(); flush stdout;
    let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
    Io.write_cnf dimacs;
    
    call_minisat ();
    Io.output_answer ();
