(* call minisat on "out.cnf", storing result in "out.txt" *)
let call_minisat cnf_file answer_file =
    let cmd = "../sat_solvers/minisat/minisat " 
              ^ cnf_file ^ " " ^ answer_file in
    print_string("Minisat exit code: " ^ string_of_int (Sys.command cmd));
    print_newline();
    flush stdout

let main instance_file problem_file cnf_file answer_file =
    let problem = open_in problem_file in
    let lexbuf = Lexing.from_channel problem in (* was stdin *)
    let instance = Io.read_instance instance_file in
    let result = Parse.main Lex.token lexbuf in
    close_in problem;
    let expanded = Expand.expand_expr result instance in
    print_string ( Expr.string_of_expr expanded );
    print_newline(); print_newline();
    let (subbed, nbvars) = Sub.sub_expr_call expanded in
    let cnf = Cnf.cnf_expr subbed in
    print_newline(); flush stdout;
    let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
    Io.write_cnf dimacs cnf_file;
    
    call_minisat cnf_file answer_file;
    Io.output_answer answer_file

(* get file names, or defaults if not given *)
let _ = 
    let num_args = Array.length Sys.argv in
    let instance_file = if num_args >= 2 then Sys.argv.(1)
                                         else "instance.txt" in
    let problem_file  = if num_args >= 3 then Sys.argv.(2)
                                         else "3col.txt"     in
    let cnf_file      = if num_args >= 4 then Sys.argv.(3)
                                         else "out.cnf"      in
    let answer_file   = if num_args >= 5 then Sys.argv.(4)
                                         else "out.txt"      in
    main instance_file problem_file cnf_file answer_file
