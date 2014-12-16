type term = 
      Var of string
    | Const of int

type terms = 
      Term of term
    | Terms of terms * term

type expr =
      And of expr * expr
    | Or of expr * expr
    | Not of expr
    | Forall of string * string * expr
    | Exists of string * string * expr
    | Pred of string * terms
    | Eq of term * term

let string_of_term t =
    match t with
    | Var v -> v
    | Const c -> string_of_int c

let rec string_of_terms ts =
    match ts with
    | Term t -> string_of_term t
    | Terms (ts,t) -> string_of_terms ts ^ " " ^ string_of_term t

let rec string_of_expr e =
    match e with
    | And (e1,e2) ->
        "(" ^ string_of_expr e1 ^ " & " ^ string_of_expr e2 ^ ")"
    | Or  (e1,e2) ->
        "(" ^ string_of_expr e1 ^ " | " ^ string_of_expr e2 ^ ")"
    | Not e1 -> 
        "(" ^ " ~ " ^ string_of_expr e1 ^ ")"
    | Forall (s1,s2,e1) ->
        "forall " ^ s1 ^ " in " ^ s2 ^ "(" ^ string_of_expr e1 ^ ")"
    | Exists (s1,s2,e1) ->
        "exists " ^ s1 ^ " in " ^ s2 ^ "(" ^ string_of_expr e1 ^ ")"
    | Pred (s1,ts) ->
        s1 ^ "(" ^ string_of_terms ts ^ ")"
    | Eq (t1,t2) ->
        string_of_term t1 ^ " = " ^ string_of_term t2
