open Expr

let num_clauses = ref 1

let dimacs_of_term t =
    match t with
    | Var v -> v
    | Const c -> string_of_int c

let rec dimacs_of_terms ts =
    match ts with
    | Terms [] -> ""
    | Terms (t :: ts) -> dimacs_of_term t ^ dimacs_of_terms (Terms ts)

let rec dimacs_of_expr e = 
    match e with
    | And (e1, e2) ->
        num_clauses := !num_clauses + 1;
        dimacs_of_expr e1 ^ " 0\n" ^ dimacs_of_expr e2
    | Or  (e1, e2) ->
        dimacs_of_expr e1 ^ " " ^ dimacs_of_expr e2 
    | Not e1 -> 
        "-" ^ dimacs_of_expr e1
    | Pred (s1, ts) ->
        s1 ^ dimacs_of_terms ts 
    (* forall, exists, eq ? Use proper error? *)
    | _ ->
        "ERROR\n"

let dimacs_of_expr_call e num_vars = 
    num_clauses := 1; (* reassign this here for clarity/correctness? *)
    let clauses = dimacs_of_expr e ^ " 0" in
    "p cnf " ^ string_of_int num_vars ^ " " ^ string_of_int !num_clauses 
    ^ "\n" ^  clauses
    (* also want comment line? *)
