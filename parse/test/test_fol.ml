(* command line options and args *)
let problem_file = ref ""
let instance_file = ref ""
let anon_args = ref 0
let verbose = ref false
let pbc = ref false
let cnf_pass = ref false
let tseitin = ref false
let sat_solver = ref "../sat_solvers/minisat/minisat" 
let cnf_file = ref "out.cnf"
let pbc_file = ref "out.pbc"
let answer_file = ref "out.txt"
let set_file file_ref name = file_ref := name
let time = ref 0.0
let nbclauses = ref 0

let time_section section_label = 
    let current_time = Sys.time() in
    Printf.printf "Time taken: %fs\n\n" (current_time -. !time);
    Printf.printf section_label;
    flush stdout; (* ? *)
    time := current_time

(* call minisat on "out.cnf", storing result in "out.txt" *)
let call_minisat _ =
    let cmd = !sat_solver ^ " " ^ !cnf_file ^ " " ^ !answer_file in
    print_string("Minisat exit code: " ^ string_of_int (Sys.command cmd));
    print_newline();
    flush stdout
    
(* print_string used for verbose output *)
let pv str = print_string (str ^ "\n\n")

exception Naively_unsat 

(* call minisat+ to convert pbc into cnf *)
let call_minisat_plus _ =
    let cmd = "../sat_solvers/minisat+/minisat+ " (* make arg *)
              ^ !pbc_file ^ " -v0 | cut -c 3- > " ^ !answer_file in
              (* ^ cut as minisat adds 2 unwanted chars *)
    let exit_code = Sys.command cmd in
    print_string("Minisat+ exit code: " ^ string_of_int (exit_code));
    print_newline();
    flush stdout;
    if exit_code = 20 then raise Naively_unsat 

let fol p_f i_f = 
  problem_file := p_f;
  instance_file := i_f;
  (*try*)
    time := Sys.time();
    Printf.printf "Parsing problem...\n";
    let problem = open_in !problem_file in
    let lexbuf = Lexing.from_channel problem in (* was stdin *)
    let parsed_problem = Parse.main Lex.token lexbuf in
    close_in problem;
    (*if !verbose then pv (Expr.string_of_expr parsed_problem); *)
    time_section "Parsing instance...\n";
    let instance = Io.read_instance !instance_file in
    time_section "Expanding problem...\n";
    let expanded = Expand.expand_expr parsed_problem instance in
    (*if !verbose then pv (Expr.string_of_expr expanded);    *)
    time_section "Substituting predicates...\n";
    let (subbed, nbvars, pred_map) = Sub.sub_expr_call expanded !pbc in
    time_section "Converting to CNF...\n";
    let (cnf, nbvars, pred_map) = 
        if !tseitin then Cnf.tseitin_cnf_expr subbed nbvars pred_map !pbc
        else (Cnf.cnf_expr subbed, nbvars, pred_map) in 
    (*if !verbose then pv (Expr.string_of_expr cnf);   *)
	if !pbc then ( 
        time_section "Converting to PBC...\n";
        let pbc = Pbc.pbc_of_expr_call cnf nbvars in
        nbclauses := Pbc.(!num_clauses);
        Io.write_cnf pbc !pbc_file;
        time_section "Running PBC-solver...\n";
        call_minisat_plus (); )
    else (
        time_section "Converting to DIMACS-CNF...\n";
        let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
        nbclauses := Dimacs.(!num_clauses);
        Io.write_cnf dimacs !cnf_file; 
        time_section "Running SAT-solver...\n";
        call_minisat (); );
    time_section "Replacing predicates...\n";
    Io.output_answer pred_map !answer_file (not !pbc);
    Printf.printf "Total running time: %fs\n\n" !time;
    flush stdout;
    (nbvars, !nbclauses)
 (* with Expr.Unexpected_expr_found (expr, str) ->
    print_string( "\n" ^ Expr.string_of_expr expr ^ " found in " ^ str ^ "\n") *)
