open Gen_graph
open Read_graph
open Test_fol

let nb_graphs = 50
let v_min = 10
let v_max = 250
let dir = "test/test_graphs/"
let k = 3
let problem_file = "problems/3col.txt"
let results_file = "test/results/results_10s_timeout_tes.csv"
let sec_limit = 10

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

let print_times (v,e,t1,t2,nbv,nbc) =
    let oc = Pervasives.open_out_gen [Open_creat; Open_text; Open_append] 
        0o640 results_file in
    Pervasives.output_string oc (
        string_of_float t1 ^ ", " ^
        string_of_float t2 ^ ", " ^
        string_of_int v    ^ ", " ^
        string_of_int e    ^ ", " ^
        string_of_int nbv  ^ ", " ^
        string_of_int nbc  ^ "\n" );
    Pervasives.close_out oc

let () =
    Random.self_init ();
    for i=1 to nb_graphs do 
        let v = (Random.int (v_max - v_min + 1)) + v_min in
        let e = (Random.int ((v * (v/5))/2) ) + 1 in
        let v_str = string_of_int v in 
        let e_str = string_of_int e in 
        print_string (v_str ^ " " ^ e_str ^ "\n" );
        Pervasives.flush stdout;
        let graph_file_name = (dir ^ v_str ^ "_" ^ e_str ^ ".graph") in
        gen_graph v e graph_file_name;
        let t1 = Unix.gettimeofday() in
        timeout read_graph (graph_file_name, k) sec_limit ();
        let t2 = Unix.gettimeofday() in
        let (nbvars, nbclauses) = fol problem_file graph_file_name in
        let t3 = Unix.gettimeofday() in
        print_times (v, e, (t2 -. t1), (t3 -. t2), nbvars, nbclauses)
    done;

