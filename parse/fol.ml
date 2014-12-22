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

(* read out.txt to find satisfying assignment (or not) *)
let output_answer () =
    let ic = open_in "out.txt" in
    try 
        let line = input_line ic in
        if line = "SAT" then (
            let line2 = input_line ic in
         (* let vars = Str.split (Str.regexp " ") line in *)
         (* map back to predicates?? *)
            print_string ("Satisfying assignment: " ^ line2 ^ "\n") )
        else if line = "UNSAT" then (
            print_string "No satisfying assignment exists\n"; )
        else
            print_string "FILE FORMAT ERROR\n"; (* throw error? *)
        flush stdout;    
        close_in ic       
    with e ->              
        close_in_noerr ic;  
        raise e              

let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let instance = Io.read_instance () in
            let result = Parse.main Lex.token lexbuf in
            let expanded = Expand.expand_expr result instance in
            print_string ( Expr.string_of_expr expanded );
            print_newline(); print_newline();
            let (subbed, nbvars) = Sub.sub_expr_call expanded in
            let cnf = Cnf.cnf_expr subbed in
            print_newline(); flush stdout;
            let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
            write_cnf dimacs;
            
            call_minisat ();
            output_answer ();
        done
    with Lex.Eof ->
        exit 0
