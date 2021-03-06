open Sub

let num_clauses = ref 1
(* should rhs be placed? true apart from after cardinality constraint *)
let place_rhs = ref true
(*rhs of pbc clause, default=1, negative preds (~x = (1-x)) change this*)
(* is this the best way to deal with negatives? *)
let rhs = ref 1

(* add clause rhs (" >= k") if needed *)
let add_rhs buff = 
    if !place_rhs then (
        Buffer.add_string buff (">= " ^ string_of_int !rhs ^ ";\n") );
    place_rhs := true

exception Not_in_nnf

let rec pbc_of_expr buff e = 
    match e with
    | And_s (e1, e2) ->
        num_clauses := !num_clauses + 1;
        rhs := 1;
        pbc_of_expr buff e1;
        add_rhs buff;
        rhs := 1;
        pbc_of_expr buff e2
    | Or_s  (e1, e2) ->
        pbc_of_expr buff e1;
        pbc_of_expr buff e2
    | Not_s (Sub_s s1 ) -> (* only predicates can be negated *) 
        rhs := !rhs - 1;
        Buffer.add_string buff ("-1 " ^ s1 ^ " ") (* was "-1*" *)
    | Not_s _ -> 
        raise Not_in_nnf
    | Sub_s s1 ->
        Buffer.add_string buff ("+1 " ^ s1 ^ " ") (* was "+1*" *)
    | Card_s (preds, eq, k) -> 
        ignore (List.map (pbc_of_expr buff) preds); (*using side effects*)
        Buffer.add_string buff (eq ^ " " ^ string_of_int k ^ ";\n");
        place_rhs := false

   (* do i need all these args for pbc? *)
let pbc_of_expr_call e num_vars = 
    num_clauses := 1; (* reassign this here for clarity/correctness? *)
    let buff = Buffer.create 1024 in (* 1024? *)
    pbc_of_expr buff e;
    add_rhs buff;
    let clauses = Buffer.contents buff in
    "* p cnf " ^ string_of_int num_vars ^ " " ^ string_of_int !num_clauses 
    ^ "\n" ^  clauses
    (* also want comment line? *)
