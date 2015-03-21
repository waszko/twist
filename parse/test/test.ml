open Gen_graph
open Read_graph
open Fol

let nb_graphs = 50
let v_min = 10
let v_max = 300
let dir = "test/test_graphs/"
let sec_limit = 60
let k = 8
let repeat = 1
let pbc = true
let cmp_res = false
(*let problem_file = "problems/" ^ string_of_int k ^ "col.txt"*)
let problem_file = "problems/clique.pbprob"
let results_file = "test/results/" 
                   ^ "test_clique2_" 
                   ^ "k-" ^ string_of_int k 
                   ^ "_vmin-" ^  string_of_int v_min 
                   ^ "_vmax-" ^ string_of_int v_max 
                   ^ "_to-" ^ string_of_int sec_limit 
                   ^ ".csv"

exception Timeout

let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)

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

let str_bool_digit b =
    match b with
    | true -> "1"
    | false -> "0"

let print_times (v,e,t1,t2,res,nbv1,nbc1,nbv2,nbc2) =
    let oc = open_out_gen [Open_creat; Open_text; Open_append] 
        0o640 results_file in
    output_string oc (
        string_of_float t1 ^ ", " ^
        string_of_float t2 ^ ", " ^
        string_of_int v    ^ ", " ^
        string_of_int e    ^ ", " ^
        str_bool_digit res ^ ", " ^
        string_of_int nbv1 ^ ", " ^
        string_of_int nbc1 ^ ", " ^
        string_of_int nbv2 ^ ", " ^
        string_of_int nbc2 ^ "\n" );
    close_out oc

exception File_format_error of string

(* read a dimacs_cnf file and return the # of vars and clauses *)
let read_dimacs file_name = 
    let ic = open_in file_name in
    let nbvars = ref 0 in
    let nbclauses = ref 0 in
    try
        while !nbvars = 0 && !nbclauses = 0 do
                let line = input_line ic in
                if line.[0] = 'p' then
                    match Str.split (Str.regexp " ") line with
                    | p::cnf::nbv::nbc::[] -> 
                        nbvars := int_of_string nbv;
                        nbclauses := int_of_string nbc
                    | _ -> raise (File_format_error "p line error")
        done; (!nbvars, !nbclauses)
    with | End_of_file -> close_in ic;
    (!nbvars, !nbclauses)

(* run SatELite to minimise a CNF file, and return the before and after
 * # of clauses and variables *)    
let call_satelite cnf_file output_file = 
    let cmd = "../sat_solvers/SatELite_v1.0_linux " 
              ^ cnf_file ^ " " ^ output_file in
    ignore (Sys.command cmd);
    let (nbv1, nbc1) = read_dimacs cnf_file in
    let (nbv2, nbc2) = read_dimacs output_file in
    (nbv1, nbc1, nbv2, nbc2)

let run_fol problem_file graph_file pbc = 
    Fol.problem_file := problem_file;
    Fol.instance_file := graph_file;
    Fol.pbc := pbc;
    Fol.run ()

exception Different_answers

let () =
    Random.self_init ();
    for i=1 to nb_graphs do 
        let v = (Random.int (v_max - v_min + 1)) + v_min in
        let e = (Random.int ((v * (v-1))/2 -1)) +1 in (* +1 to not get 0 *)
        let v_str = string_of_int v in 
        let e_str = string_of_int e in 
        print_string (v_str ^ " " ^ e_str ^ "\n" );
        flush stdout;
        let graph_file_name = (dir ^ v_str ^ "_" ^ e_str ^ ".graph") in
        for j=1 to repeat do
            gen_graph v e k graph_file_name;
            let t1 = Unix.gettimeofday() in
           (* let result1 = timeout read_graph (graph_file_name, k) 
                              sec_limit false in *)
            let result1 = false in (* temp *)
            let t2 = Unix.gettimeofday() in
            run_fol problem_file graph_file_name pbc;
            let result2 = !Io.answer in 
            if cmp_res && result1 != result2 then raise Different_answers;
            let t3 = Unix.gettimeofday() in
            let (nbv1,nbc1,nbv2,nbc2) = call_satelite "out.cnf" "red.cnf" in
            print_times 
                (v, e, (t2-.t1), (t3-.t2), result2, nbv1, nbc1, nbv2, nbc2)
        done;
    done;
