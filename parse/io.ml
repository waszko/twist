module String_map = Map.Make (String);;

(* reads instance sets into map from name to string list list *)
let read_instance file_name =
    let ic = open_in file_name in
    let sets_map = ref String_map.empty in (* map from set name to set *)
    try
        while true do
            let line = input_line ic in 
            let (name::set) = Str.split (Str.regexp " ") line in (*[]err*)
            if String_map.mem name !sets_map then
                let sets = String_map.find name !sets_map in
                sets_map := String_map.add name (set::sets) !sets_map
            else
                sets_map := String_map.add name [set] !sets_map
        done; !sets_map
    with End_of_file ->
        close_in ic;
        !sets_map 

(* write dimacs_cnf string to file *)
let write_cnf str file_name =
    let oc = open_out file_name in
    Printf.fprintf oc "%s" str;
    close_out oc


(* convert list of pred substitutions into string of all preds (str) *)
let rec get_preds sub_list map str =
    match sub_list with
    | [] -> str
    | last::[] -> (* skip last element which is 0 *)
                get_preds [] map str
    | hd::tl -> (* if preds are false, they are preceded by a '-' *)
                let neg = (hd.[0] = '-') in
                let pre = (if neg then " -" else " ") in
                let sub = (if not neg then hd else
                           String.sub hd 1 (String.length hd - 1) ) in
                get_preds tl map ( str ^ pre ^
                    (Sub.Int_map.find (int_of_string sub) map) )

(* read output of sat-solver to find satisfying assignment (or not) *)
let output_answer pred_map file_name =
    let ic = open_in file_name in
    try 
        let line = input_line ic in
        if line = "SAT" then (
            let line2 = input_line ic in
            let vars = Str.split (Str.regexp " ") line2 in
            (* map back to predicates *)
            let preds = get_preds vars pred_map "" in
            print_string ("Satisfying assignment:" ^ preds ^ "\n") )
        else if line = "UNSAT" then (
            print_string "No satisfying assignment exists\n"; )
        else
            print_string "FILE FORMAT ERROR\n"; (* throw error? *)
        flush stdout;    
        close_in ic       
    with e ->              
        close_in_noerr ic;  
        raise e              
