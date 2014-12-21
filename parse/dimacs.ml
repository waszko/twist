open Expr

let nbclauses = ref 1 (* is using a ref good? *)

let dimacs_of_term t =
    match t with
    | Var v -> v
    | Const c -> string_of_int c

let rec dimacs_of_terms ts =
    match ts with
    | Terms [] -> "" (* have single elem case to remove trailing spaces? *)
    | Terms (t::ts) -> dimacs_of_term t ^ dimacs_of_terms (Terms ts)

let rec dimacs_of_expr e = 
    match e with
    | And (e1,e2) ->
        nbclauses := !nbclauses + 1; (* is this good? *)
        dimacs_of_expr e1 ^ " 0\n" ^ dimacs_of_expr e2
    | Or  (e1,e2) ->
        dimacs_of_expr e1 ^ " " ^ dimacs_of_expr e2 
    | Not e1 -> 
        "-" ^ dimacs_of_expr e1
    | Pred (s1,ts) ->
        s1 ^ dimacs_of_terms ts 
    (* forall, exists, eq ? Use proper error? *)
    | _ ->
        "ERROR\n"

let dimacs_of_expr_call e nbvars = 
    let clauses = dimacs_of_expr e ^ " 0" in
    "p cnf " ^ string_of_int nbvars ^ " " ^ string_of_int !nbclauses ^ "\n"
        ^  clauses
    (* also want comment line? *)
