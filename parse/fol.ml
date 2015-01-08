let time_section section_label prev_time = 
    let current_time = Sys.time() in
    Printf.printf "Time taken: %fs\n\n" (current_time -. prev_time);
    Printf.printf section_label;
    flush stdout; (* ? *)
    current_time

(* call minisat on "out.cnf", storing result in "out.txt" *)
let call_minisat cnf_file answer_file =
    let cmd = "../sat_solvers/minisat/minisat " 
              ^ cnf_file ^ " " ^ answer_file in
    print_string("Minisat exit code: " ^ string_of_int (Sys.command cmd));
    print_newline();
    flush stdout

let main instance_file problem_file cnf_file answer_file =
  try
    let t = Sys.time() in
    Printf.printf "Parsing problem...\n";
    let problem = open_in problem_file in
    let lexbuf = Lexing.from_channel problem in (* was stdin *)
    let parsed_problem = Parse.main Lex.token lexbuf in
    close_in problem;
    (*print_string ( Expr.string_of_expr parsed_problem ^ "\n\n" ); *)
    let t = time_section "Parsing instance...\n" t in
    let instance = Io.read_instance instance_file in
    let t = time_section "Expanding problem...\n" t in
    let expanded = Expand.expand_expr parsed_problem instance in
    (*print_string ( Expr.string_of_expr expanded ^ "\n\n" );    *)
    let t = time_section "Substituting predicates...\n" t in
    let (subbed, nbvars, pred_map) = Sub.sub_expr_call expanded in
    let t = time_section "Converting to CNF...\n" t in
    let cnf = Cnf.cnf_expr subbed in
    (*print_string ( Expr.string_of_expr cnf ^ "\n\n" );   *)
    flush stdout; (* ? *)
    let t = time_section "Converting to DIMACS-CNF...\n" t in
    let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
    Io.write_cnf dimacs cnf_file;
    let t = time_section "Running SAT-solver...\n" t in
    call_minisat cnf_file answer_file;
    let t = time_section "Replacing predicates...\n" t in
    Io.output_answer pred_map answer_file;
    Printf.printf "Total running time: %fs\n\n" t;
    flush stdout
  with Expr.Unexpected_expr_found (expr, str) ->
    print_string( "\n" ^ Expr.string_of_expr expr ^ " found in " ^ str ^ "\n")

(* get file names, or defaults if not given *)
let _ = 
    let num_args = Array.length Sys.argv in
    let instance_file = if num_args >= 2 then Sys.argv.(1)
                                         else "graphs/basic.txt" in
    let problem_file  = if num_args >= 3 then Sys.argv.(2)
                                         else "problems/3col.txt"  in
    let cnf_file      = if num_args >= 4 then Sys.argv.(3)
                                         else "out.cnf"      in
    let answer_file   = if num_args >= 5 then Sys.argv.(4)
                                         else "out.txt"      in
    main instance_file problem_file cnf_file answer_file
