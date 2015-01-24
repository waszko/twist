type term = 
      Var of string
    | Const of int

type terms = 
      Terms of term list

type expr =
      And of expr * expr
    | Or of expr * expr
    | Not of expr
    | Forall of terms * string * expr
    | Exists of terms * string * expr
    | Pred of string * terms
    | Eq of term * term
    | True
    | False
    | Card1 of string * string * string
    | Card2 of expr list * int

(* when found an unexpected 'expr' in function 'string' *)
exception Unexpected_expr_found of (expr * string)

let string_of_term t =
    match t with
    | Var v -> v
    | Const c -> string_of_int c

let rec string_of_terms ts =
    match ts with
    | Terms [] -> ""
    | Terms (hd :: tl) -> 
        string_of_term hd ^ " " ^ string_of_terms (Terms tl)

let rec string_of_pred_list ps =
    match ps with
    | [] -> ""
    | Pred (s1, ts) :: tl ->
        s1 ^ "("  ^ string_of_terms ts ^ ") " ^ string_of_pred_list tl
    | e :: tl -> 
        raise (Unexpected_expr_found (e, "Expr.string_of_pred_list"))

let rec string_of_expr e =
    match e with
    | And (e1, e2) ->
        "(" ^ string_of_expr e1 ^ " & " ^ string_of_expr e2 ^ ")"
    | Or  (e1, e2) ->
        "(" ^ string_of_expr e1 ^ " | " ^ string_of_expr e2 ^ ")"
    | Not e1 -> 
        "(" ^ " ~ " ^ string_of_expr e1 ^ ")"
    | Forall (ts, s1, e1) ->
        "forall " ^ string_of_terms ts ^ " in " 
        ^ s1 ^ "(" ^ string_of_expr e1 ^ ")"
    | Exists (ts, s1, e1) ->
        "exists " ^ string_of_terms ts ^ " in " 
        ^ s1 ^ "(" ^ string_of_expr e1 ^ ")"
    | Pred (s1, ts) ->
        s1 ^ "(" ^ string_of_terms ts ^ ")"
    | Eq (t1, t2) ->
        string_of_term t1 ^ " = " ^ string_of_term t2
    | True -> "true"
    | False -> "false"
    | Card1 (p, s, k) -> "(|" ^ p ^ " of " ^ s ^"| = " ^ k ^ ")"
    | Card2 (ps, k) -> 
        "([" ^ string_of_pred_list ps ^ "] = " ^ string_of_int k ^")"
