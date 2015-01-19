(* command line options and args *)
let problem_file = ref ""
let instance_file = ref ""
let anon_args = ref 0
let verbose = ref false
let sat_solver = ref "../sat_solvers/minisat/minisat" 
let cnf_file = ref "out.cnf"
let answer_file = ref "out.txt"
let set_file file_ref name = file_ref := name

let set_anon_arg file =
   anon_args := !anon_args + 1;
   match !anon_args with
   | 1 -> problem_file := file
   | 2 -> instance_file := file
   | _ -> print_string ("Extra argument: " ^ file ^ "\n")

let option_spec = [
     ("-v", Arg.Set verbose, "enable verbose output"); 
     ("-s", Arg.String (set_file sat_solver), 
         "specify SAT-solver location (default=" ^ !sat_solver ^ ")"); 
     ("-c", Arg.String (set_file cnf_file), 
         "specify CNF output file (default=" ^ !cnf_file ^ ")"); 
     ("-a", Arg.String (set_file answer_file), 
         "specify SAT-solver output file (default=" ^ !answer_file ^ ")"); 
    ] 

let usage_msg = "Usage: fol.byte|native [options] [<problem file>] \
                 [<instance file>]\nOptions are:"

let _ = Arg.parse option_spec set_anon_arg usage_msg

let time_section section_label prev_time = 
    let current_time = Sys.time() in
    Printf.printf "Time taken: %fs\n\n" (current_time -. prev_time);
    Printf.printf section_label;
    flush stdout; (* ? *)
    current_time

(* call minisat on "out.cnf", storing result in "out.txt" *)
let call_minisat _ =
    let cmd = !sat_solver ^ " " ^ !cnf_file ^ " " ^ !answer_file in
    print_string("Minisat exit code: " ^ string_of_int (Sys.command cmd));
    print_newline();
    flush stdout
    
(* print_string only if verbose enabled *)
let print_verbose str = if !verbose then print_string str

let _ =
  try
    assert (!anon_args >= 2); (* give better output for this *)
    let t = Sys.time() in
    Printf.printf "Parsing problem...\n";
    let problem = open_in !problem_file in
    let lexbuf = Lexing.from_channel problem in (* was stdin *)
    let parsed_problem = Parse.main Lex.token lexbuf in
    close_in problem;
    print_verbose ( Expr.string_of_expr parsed_problem ^ "\n\n" ); 
    let t = time_section "Parsing instance...\n" t in
    let instance = Io.read_instance !instance_file in
    let t = time_section "Expanding problem...\n" t in
    let expanded = Expand.expand_expr parsed_problem instance in
    print_verbose ( Expr.string_of_expr expanded ^ "\n\n" );    
    let t = time_section "Substituting predicates...\n" t in
    let (subbed, nbvars, pred_map) = Sub.sub_expr_call expanded in
    let t = time_section "Converting to CNF...\n" t in
    let cnf = Cnf.cnf_expr subbed in
    print_verbose ( Expr.string_of_expr cnf ^ "\n\n" );   
    flush stdout; (* ? *)
    let t = time_section "Converting to DIMACS-CNF...\n" t in
    let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
    Io.write_cnf dimacs !cnf_file;
    let t = time_section "Running SAT-solver...\n" t in
    call_minisat ();
    let t = time_section "Replacing predicates...\n" t in
    Io.output_answer pred_map !answer_file;
    Printf.printf "Total running time: %fs\n\n" t;
    flush stdout
  with Expr.Unexpected_expr_found (expr, str) ->
    print_string( "\n" ^ Expr.string_of_expr expr ^ " found in " ^ str ^ "\n")

