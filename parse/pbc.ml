open Expr

let num_clauses = ref 1

(*rhs of pbc clause, default=1, negative preds (~x = (1-x)) change this*)
(* is this the best way to deal with negatives? *)
let rhs = ref 1

let pbc_of_term t buff =
    match t with
    | Var v -> Buffer.add_string buff ("1*" ^ v)
    | Const c -> Buffer.add_string buff (string_of_int c)

let rec pbc_of_terms ts buff =
    match ts with
    | Terms [] -> () 
    | Terms (t :: ts) ->
        pbc_of_term t buff;
        pbc_of_terms (Terms ts) buff

let rec pbc_of_expr e buff = 
    match e with
    | And (e1, e2) ->
        num_clauses := !num_clauses + 1;
        rhs := 1;
        pbc_of_expr e1 buff;
        Buffer.add_string buff (" >= " ^ string_of_int !rhs ^ ";\n") ;
        rhs := 1;
        pbc_of_expr e2 buff
    | Or  (e1, e2) ->
        pbc_of_expr e1 buff;
        Buffer.add_string buff " ";
        pbc_of_expr e2 buff
    | Not (Pred (s1, ts)) -> (* only predicates can be negated *) 
        rhs := !rhs - 1;
        Buffer.add_string buff "-";
        pbc_of_terms ts buff
   | Pred (s1, ts) ->
        Buffer.add_string buff "+";
        pbc_of_terms ts buff
    | Not _ | Forall _ | Exists _ | Eq _ | True | False ->
        raise (Unexpected_expr_found (e, "Pbc.pbc_of_expr"))

   (* do i need all these args for pbc? *)
let pbc_of_expr_call e num_vars = 
    num_clauses := 1; (* reassign this here for clarity/correctness? *)
    let buff = Buffer.create 1024 in (* 1024? *)
    pbc_of_expr e buff;
    let clauses = Buffer.contents buff ^ " >= "^ string_of_int !rhs ^";" in
    "* p cnf " ^ string_of_int num_vars ^ " " ^ string_of_int !num_clauses 
    ^ "\n" ^  clauses
    (* also want comment line? *)
