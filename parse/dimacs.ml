open Expr

let dimacs_of_term t =
    match t with
    | Var v -> v
    | Const c -> string_of_int c

let rec dimacs_of_terms ts =
    match ts with
    | Term t -> dimacs_of_term t
    | Terms (ts,t) -> dimacs_of_terms ts ^ " " ^ dimacs_of_term t

let rec dimacs_of_expr e =
    match e with
    | And (e1,e2) ->
        dimacs_of_expr e1 ^ " 0\n" ^ dimacs_of_expr e2
    | Or  (e1,e2) ->
        dimacs_of_expr e1 ^ " " ^ dimacs_of_expr e2 
    | Not e1 -> 
        "-" ^ dimacs_of_expr e1
    | Pred (s1,ts) ->
        s1 ^ "(" ^ dimacs_of_terms ts ^ ")"
    (* forall, exists, eq ? Use proper error? *)
    | _ ->
        "ERROR\n"
