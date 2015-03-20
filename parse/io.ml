module String_map = Map.Make (String);;
(* Expand's Str_list_set used to avoid circular dependencies.. *)

(* currently used only in sat-solver output file *) 
exception File_format_error of string

(* reads instance sets into map from name to string list list *)
let read_instance file_name =
    let ic = open_in file_name in
    let sets_map = ref String_map.empty in (* map from set name to set *)
    try
        while true do
            let line = input_line ic in 
         (* let (name::set) = Str.split (Str.regexp " ") line in *)
            match Str.split (Str.regexp " ") line with
            | [] -> () (* skip empty lines *) 
            | (name::elem) ->
                if String_map.mem name !sets_map then (
                    (* add elem to the found string list set *)
                    let set = String_map.find name !sets_map in
                    let set = Expand.Str_list_set.add elem set in
                    sets_map:= String_map.add name set !sets_map )
                else (
                    (* create a new set of string lists (elems) *)
                    let set = Expand.Str_list_set.singleton elem in 
                    sets_map := String_map.add name set !sets_map )
        done; !sets_map
    with 
    | End_of_file ->
        close_in ic;
        !sets_map

(* write dimacs_cnf string to file *)
let write_cnf str file_name =
    let oc = open_out file_name in
    Printf.fprintf oc "%s" str;
    close_out oc

let skip_last = ref false (* skip last pred in solution? *)

(* convert list of pred substitutions into string of all preds (str) *)
(* not used currently (replaced by get_pos_preds below) *)
let rec get_preds sub_list map str =
    match sub_list with
    | [] -> str
    | hd::tl -> (* if preds are false, they are preceded by a '-' *)
          if !skip_last && tl = [] then get_preds [] map str else 
          let neg = (hd.[0] = '-') in
          let pre = (if neg then " -" else " ") in
          let sub = (if not neg then hd else
                     String.sub hd 1 (String.length hd - 1) ) in
          get_preds tl map ( str ^ pre ^ (Sub.String_map.find sub map) )

(* like above but only returns positive predicates *)
let rec get_pos_preds sub_list map str =
    match sub_list with
    | [] -> str
    | hd::tl -> (* if preds are false, they are preceded by a '-' *)
          if !skip_last && tl = [] then get_pos_preds [] map str else 
          let neg = (hd.[0] = '-') in
          let pred = if neg then "" else Sub.String_map.find hd map in
          (* space valid predicates (e.g. not tseitin ones): *)
          let pred = if not (pred = "") then " " ^ pred else pred in
          get_pos_preds tl map (str ^ pred )

(* read output of sat-solver to find satisfying assignment (or not) *)
let answer = ref false
let output_answer pred_map file_name trailing_0 =
    skip_last := trailing_0; (* answer file has a trailing 0 predicate *)
    let ic = open_in file_name in
    try 
        let line = input_line ic in
        if (line = "SAT" || line = "SATISFIABLE") then (
            answer := true;
            let line2 = input_line ic in
            let vars = Str.split (Str.regexp " ") line2 in
            (* map back to predicates *)
            let preds = get_pos_preds vars pred_map "" in
            print_string ("Satisfying assignment:" ^ preds ^ "\n") )
        else if (line = "UNSAT" || line = "UNSATISFIABLE") then (
            answer := false;
            print_string "No satisfying assignment exists\n"; )
        else (
            raise (File_format_error file_name) );
        flush stdout;    
        close_in ic       
    with e ->              
        close_in_noerr ic;  
        raise e              
