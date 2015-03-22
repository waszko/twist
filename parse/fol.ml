(* command line options and args *)
let problem_file = ref ""
let instance_file = ref ""
let anon_args = ref 0
let verbose = ref false
let pbc = ref false
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

let usage_msg = "Usage: fol.byte|native [options] <problem file> \
                 <instance file>\n"

let _ = Arg.parse option_spec set_anon_arg (usage_msg ^ "Options are:")

let time_section section_label = 
    let current_time = Unix.gettimeofday() in
    if !verbose then print_newline(); 
    Printf.printf "Time taken: %fs\n" (current_time -. !time);
    if !verbose then print_newline(); 
    Printf.printf section_label;
    flush stdout; (* ? *)
    time := current_time

(* call minisat on "out.cnf", storing result in "out.txt" *)
let call_minisat _ =
    print_string "\n"; flush stdout;
    let cmd = !sat_solver ^ " " ^ !cnf_file ^ " " ^ !answer_file in
    ignore (Sys.command cmd);
    flush stdout
    
(* print_string used for verbose output *)
let pv str = print_string ("\n" ^ str)

exception Naively_unsat 

(* call minisat+ to convert pbc into cnf *)
let call_minisat_plus _ =
    let cmd = "../sat_solvers/minisat+/minisat+ " (* make arg *)
              ^ !pbc_file ^ " -v0 | cut -c 3- > " ^ !answer_file in
              (* ^ cut as minisat adds 2 unwanted chars (hacky) *)
    let exit_code = Sys.command cmd in
    flush stdout;
    if exit_code = 20 then raise Naively_unsat 

let run _ =
  try
    time := Unix.gettimeofday();
    let start_time = !time in
    Printf.printf "Parsing problem............";
    let problem = open_in !problem_file in
    let lexbuf = Lexing.from_channel problem in (* was stdin *)
    let (given_sets, parsed_problem) = Parse.main Lex.token lexbuf in
    close_in problem;
    if !verbose then pv (Expr.string_of_expr parsed_problem); 
    time_section "Parsing instance...........";
    let instance = Io.read_instance !instance_file in
    time_section "Expanding problem..........";
    let expanded = Expand.expand_expr parsed_problem instance in
    if !verbose then pv (Expand.string_of_expr_e expanded);    
    time_section "Propagating Booleans.......";
    let eq = Eq.call_eq expanded given_sets instance in
    if !verbose then pv (Expand.string_of_expr_e eq);    
    time_section "Substituting predicates....";
    let (subbed, nbvars, pred_map) = Sub.sub_expr_call eq !pbc in
    if !verbose then pv (Sub.string_of_expr_s subbed);    
    time_section "Converting to CNF..........";
    let (cnf, nbvars, pred_map) = 
        if !tseitin then Cnf.tseitin_cnf_expr subbed nbvars pred_map !pbc
        else (Cnf.cnf_expr subbed, nbvars, pred_map) in 
    if !verbose then pv (Sub.string_of_expr_s cnf);   
	if !pbc then ( 
        time_section "Converting to PBC..........";
        let pbc = Pbc.pbc_of_expr_call cnf nbvars in
        Io.write_cnf pbc !pbc_file;
        time_section "Running PBC-solver.........";
        call_minisat_plus (); )
    else (
        time_section "Converting to DIMACS-CNF...";
        let dimacs = Dimacs.dimacs_of_expr_call cnf nbvars in
        Io.write_cnf dimacs !cnf_file; 
        time_section "Running SAT-solver.........";
        call_minisat (); );
    time_section "Replacing predicates.......";
    Io.output_answer pred_map !answer_file (not !pbc);
    Printf.printf "Total running time: %fs\n\n" (!time -. start_time);
    flush stdout
  with Expr.Unexpected_expr_found (expr, str) ->
    print_string( "\n" ^ Expr.string_of_expr expr ^ " found in " ^ str ^ "\n")

let _ = 
    if !anon_args >= 2 then run ()
    else print_string usage_msg
