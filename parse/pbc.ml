open Expr

let num_clauses = ref 1
(* should rhs be placed? true apart from after cardinality constraint *)
let place_rhs = ref true
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

(* add cardinality constraint of the form P(1) + P(2) + P(3) >= k *)
let rec card_const preds k buff =
    match preds with (* im converting k to int then back to string... *)
    | [] -> 
        Buffer.add_string buff (">= " ^ string_of_int k ^ ";\n") ;
        place_rhs := false
    | Pred(_,ts) :: tl -> 
        Buffer.add_string buff "+";
        pbc_of_terms ts buff;
        Buffer.add_string buff " ";
        card_const tl k buff
    | e :: tl -> raise (Unexpected_expr_found (e, "Pbc.card_const"))

(* add clause rhs (" >= k") if needed *)
let add_rhs buff = 
    if !place_rhs then (
        Buffer.add_string buff (" >= " ^ string_of_int !rhs ^ ";\n") );
    place_rhs := true

let rec pbc_of_expr e buff = 
    match e with
    | And (e1, e2) ->
        num_clauses := !num_clauses + 1;
        rhs := 1;
        pbc_of_expr e1 buff;
        add_rhs buff;
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
    | Card2 (preds, k) -> card_const preds k buff
    | Not _ | Forall _ | Exists _ | Eq _ | True | False | Card1 _ ->
        raise (Unexpected_expr_found (e, "Pbc.pbc_of_expr"))

   (* do i need all these args for pbc? *)
let pbc_of_expr_call e num_vars = 
    num_clauses := 1; (* reassign this here for clarity/correctness? *)
    let buff = Buffer.create 1024 in (* 1024? *)
    pbc_of_expr e buff;
    add_rhs buff;
    let clauses = Buffer.contents buff in
    "* p cnf " ^ string_of_int num_vars ^ " " ^ string_of_int !num_clauses 
    ^ "\n" ^  clauses
    (* also want comment line? *)
