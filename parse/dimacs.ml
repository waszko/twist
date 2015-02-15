open Sub

let num_clauses = ref 1

(*
let dimacs_of_term t buff =
    match t with
    | Var v -> Buffer.add_string buff v
    | Const c -> Buffer.add_string buff (string_of_int c)

let rec dimacs_of_terms ts buff =
    match ts with
    | Terms [] -> () 
    | Terms (t :: ts) ->
        dimacs_of_term t buff;
        dimacs_of_terms (Terms ts) buff
*)

let rec dimacs_of_expr e buff = 
    match e with
    | And_s (e1, e2) ->
        num_clauses := !num_clauses + 1;
        dimacs_of_expr e1 buff;
        Buffer.add_string buff " 0\n";
        dimacs_of_expr e2 buff
    | Or_s  (e1, e2) ->
        dimacs_of_expr e1 buff;
        Buffer.add_string buff " ";
        dimacs_of_expr e2 buff
    | Not_s e1 -> 
        Buffer.add_string buff "-";
        dimacs_of_expr e1 buff
    | Sub_s s1 ->
        Buffer.add_string buff s1
    (* Card_s *)

(* changed to use string buffer as is much faster than string concat *)
let dimacs_of_expr_call e num_vars = 
    num_clauses := 1; (* reassign this here for clarity/correctness? *)
    let buff = Buffer.create 1024 in (* 1024? *)
    dimacs_of_expr e buff;
    let clauses = Buffer.contents buff ^ " 0" in
    "p cnf " ^ string_of_int num_vars ^ " " ^ string_of_int !num_clauses 
    ^ "\n" ^  clauses
    (* also want comment line? *)
