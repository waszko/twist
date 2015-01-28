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

let set_anon_arg file =
   anon_args := !anon_args + 1;
   match !anon_args with
   | 1 -> problem_file := file
   | 2 -> instance_file := file
   | _ -> print_string ("Extra argument: " ^ file ^ "\n")

let option_spec = [
     ("-v", Arg.Set verbose, "enable verbose output"); 
     ("-p", Arg.Set pbc, "enable pseudo-boolean constraints"); 
     ("-x", Arg.Set cnf_pass, "enable additional pass of cnf conversion"); 
     ("-t", Arg.Set tseitin, "use Tseitin method to convert to cnf"); 
     ("-s", Arg.String (set_file sat_solver), 
         "specify SAT-solver location (default=" ^ !sat_solver ^ ")"); 
     ("-c", Arg.String (set_file cnf_file), 
         "specify CNF output file (default=" ^ !cnf_file ^ ")"); 
     ("-b", Arg.String (set_file pbc_file), 
         "specify PBC output file (default=" ^ !pbc_file ^ ")"); 
     ("-a", Arg.String (set_file answer_file), 
         "specify SAT-solver output file (default=" ^ !answer_file ^ ")"); 
    ] 

let usage_msg = "Usage: fol.byte|native [options] [<problem file>] \
                 [<instance file>]\nOptions are:"

let _ = Arg.parse option_spec set_anon_arg usage_msg

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
    
(* print_string only if verbose enabled *)
let print_verbose str = if !verbose then print_string str

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

let _ =
  try
    assert (!anon_args >= 2); (* give better output for this *)
    time := Sys.time();
    Printf.printf "Parsing problem...\n";
    let problem = open_in !problem_file in
    let lexbuf = Lexing.from_channel problem in (* was stdin *)
    let parsed_problem = Parse.main Lex.token lexbuf in
    close_in problem;
    print_verbose ( Expr.string_of_expr parsed_problem ^ "\n\n" ); 
    let parsed_problem = if !cnf_pass then ( (* does this act help? *)
        time_section "Initial CNF pass...\n";
        let cnf_problem = Cnf.cnf_expr parsed_problem in
        print_verbose ( Expr.string_of_expr cnf_problem ^ "\n\n" ); 
        cnf_problem) else parsed_problem in
    time_section "Parsing instance...\n";
    let instance = Io.read_instance !instance_file in
    time_section "Expanding problem...\n";
    let expanded = Expand.expand_expr parsed_problem instance in
    print_verbose ( Expr.string_of_expr expanded ^ "\n\n" );    
    time_section "Substituting predicates...\n";
    let (subbed, nbvars, pred_map) = Sub.sub_expr_call expanded !pbc in
    time_section "Converting to CNF...\n";
    let (cnf, nbvars) = if !tseitin then Cnf.tseitin_cnf_expr subbed nbvars
                        else (Cnf.cnf_expr subbed, nbvars) in 
    print_verbose ( Expr.string_of_expr cnf ^ "\n\n" );   
    flush stdout; (* ? *)
	if !pbc then ( (* do t's work in this 'if then else' block? *)
        time_section "Converting to PBC...\n";
        let pbc = Pbc.pbc_of_expr_call cnf nbvars in
        Io.write_cnf pbc !pbc_file;
        time_section "Running PBC-solver...\n";
        call_minisat_plus (); )
    else (
        time_section "Converting to DIMACS-CNF...\n";
        let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
        Io.write_cnf dimacs !cnf_file; 
        time_section "Running SAT-solver...\n";
        call_minisat (); );
    time_section "Replacing predicates...\n";
    Io.output_answer pred_map !answer_file (not !pbc);
    Printf.printf "Total running time: %fs\n\n" !time;
    flush stdout
  with Expr.Unexpected_expr_found (expr, str) ->
    print_string( "\n" ^ Expr.string_of_expr expr ^ " found in " ^ str ^ "\n")

