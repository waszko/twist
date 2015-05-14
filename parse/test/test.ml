let generate = true  (* should new random graphs be generated or read *)

(* generate = true : Graph generation variables: *)
let nb_graphs = 100   (* number of graphs to generate *)
let v_min = 50        (* min # of vertices *)
let v_max = 50       (* max # of vertices *)
let write_dir = "test/graphs/"

(* generate = false : Graph reading variables *)
let read_dir = "../instances/3col/assat_small/" 
let read_dir_name = "assat3col"

(* General use variables: *)
let problem = "3col"
let k = 3
let pbc = false
let tseitin = false
let sec_limit = 10    (* timeout limit in seconds *)
let repeat = 20       (* how many times each graph size should be tested *)
let compare = false   (* compare my system against OCamlGraph alg *)
let reduce = false    (* add size of reduced SAT files to output *)
let output = ref []
let problem_file = "problems/" ^ problem ^ 
                   if pbc then ".pbprob" else ".prob"
let results_file = "test/results/" 
                   ^ problem
                   ^ (if pbc then "_k-" ^ string_of_int k else "" )
                   ^ (if generate then "_vmin-" ^  string_of_int v_min ^
                                       "_vmax-" ^ string_of_int v_max
                      else "_dir-" ^ read_dir_name )
                   ^ "_to-" ^ string_of_int sec_limit 
                   ^ ".csv"

exception Timeout
exception Different_answers

let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)

(* times out function f after a given time *)
let timeout f (arg1, arg2) time default_value = 
    let old_behaviour = Sys.signal Sys.sigalrm sigalrm_handler in
    let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behaviour in
    ignore (Unix.alarm time);
    try
        let res = f arg1 arg2 in 
        reset_sigalrm ();
        res
    with exc -> reset_sigalrm ();
                if exc = Timeout then default_value else raise exc

(* boolean digits to use in output *)
let str_bool_digit b = if b then "1" else "0"

(* appends output list as a new line in the output file *)
let print_output _ =
    let oc = open_out_gen [Open_creat; Open_text; Open_append] 
        0o640 results_file in
    output_string oc ((String.concat ", " (List.rev !output)) ^ "\n");
    close_out oc

(* add # of varaibles and clauses in cnf to output *)
let analyse_cnf _ = 
    let nbv1 = if tseitin then !Cnf.n else !Sub.n in
    let nbc1 = if pbc then !Pbc.num_clauses else !Dimacs.num_clauses in 
    output := (string_of_int nbc1)::(string_of_int nbv1)::!output;
    if reduce then ( (* doesnt work with pbc! *)
        (* run minisat preprocessor to minimise a CNF file, and add 
         * minimised # of clauses and variables to the output *)
        let cnf_file = if pbc then "out.pbc" else "out.cnf" in
        let reduced_file = "reduced.cnf" in
        let cmd = "minisat " ^ cnf_file ^ " -dimacs=" ^ reduced_file in
        ignore (Sys.command cmd);
        let (nbv2, nbc2) = Read_graph.get_dimacs_size reduced_file in
        output := (string_of_int nbc2)::(string_of_int nbv2)::!output )

(* pass parameters to my system and run it *)
let run_twist problem_file graph_file = 
    Twist.problem_file := problem_file;
    Twist.instance_file := graph_file;
    Twist.pbc := pbc;
    Twist.tseitin := tseitin;
    Twist.run ()

(* test a given graph *)
let test_graph graph_file =
    let t0 = Unix.gettimeofday() in
    let (result1, t1) = if compare then (
        (* run ocamlgraph algorithm and record time & result *)
        let result1 = timeout Graph_algs.solve (graph_file, (problem, k)) 
                      sec_limit false in
        let t1 = Unix.gettimeofday() in
        output := (string_of_float (t1-.t0)) :: !output;
        (result1, t1) )
    else (false, t0) in
    timeout run_twist (problem_file, graph_file) sec_limit ();
    let result2 = !Io.answer in 
    let t2 = Unix.gettimeofday() in
    if compare && (result1 != result2) && 
       (max (t1-.t0) (t2-.t1) < float_of_int sec_limit) then 
           raise Different_answers;
    output := (string_of_float (t2-.t1)) :: !output;
    output := (str_bool_digit result2) :: !output;
    analyse_cnf ();
    print_output ()

(* test each given file *)
let process dir file =
    let path = Filename.concat dir file in
    (* ignore directories *)
    if not (Sys.is_directory path) then (
        print_endline file;
        let (v, e) = Read_graph.get_graph_size path in
        for i=1 to repeat do
            output := [string_of_int e; string_of_int v; file];
            test_graph path 
        done )

let () =
    if generate then ( 
        (* generate new random graphs to test *)
        Random.self_init();
        for i=1 to nb_graphs do
            let v = (Random.int (v_max - v_min + 1)) + v_min in
            let e = (Random.int ((v * (v-1))/2 -1)) +1 in (* at least 1 *)
            let v_str = string_of_int v in 
            let e_str = string_of_int e in 
            print_string (v_str ^ " " ^ e_str ^ "\n" );
            flush stdout;
            let graph_file=(write_dir ^ v_str ^ "_" ^ e_str ^ ".graph") in
            for j=1 to repeat do
                output := [e_str; v_str]; 
                Gen_graph.gen_graph v e k graph_file;
                test_graph graph_file
            done;
         done )
    else ( 
      (* test each graph in the given directory *)
      let files = Sys.readdir read_dir in
      Array.iter (process read_dir) files )
