open Expr

(* expanded expression - no quantifiers, true and false added (for eq.ml), 
 * and Card now contains a list of predicates *)
type expr_e =
      And_e of expr_e * expr_e
    | Or_e of expr_e * expr_e
    | Not_e of expr_e
    | Pred_e of string * terms
    | Eq_e of term * term
    | True_e
    | False_e
    | Card_e of expr_e list * string * int

exception Unexpected_expr_found of (expr_e * string) (* ? *)

let rec string_of_expr_e e =
    match e with
    | And_e (e1, e2) ->
        "(" ^ string_of_expr_e e1 ^ " & " ^ string_of_expr_e e2 ^ ")"
    | Or_e  (e1, e2) ->
        "(" ^ string_of_expr_e e1 ^ " | " ^ string_of_expr_e e2 ^ ")"
    | Not_e e1 -> 
        "~" ^ string_of_expr_e e1
    | Pred_e (s1, ts) ->
        s1 ^ "(" ^ string_of_terms ts ^ ")"
    | Eq_e (t1, t2) ->
        string_of_term t1 ^ " = " ^ string_of_term t2
    | True_e -> "true"
    | False_e -> "false"
    | Card_e (ps, eq, k) -> 
        "([" ^ String.concat " " (List.map string_of_expr_e ps)
             ^ "] " ^ eq ^ " " ^ string_of_int k ^")"

(* expansion has been attempted on an empty set (return set name?) *)
exception Set_empty 

let replace_term a b t = 
    match t with
    | Const c -> Const c
    | Var v -> if (Var v) = a then Var b else Var v

let rec replace_terms a b ts =
    match ts with
    | Terms ts -> Terms ( List.map (replace_term a b) ts )

let rec replace_expr a b e =
    match e with
    | And_e (e1, e2) -> And_e (replace_expr a b e1, replace_expr a b e2)
    | Or_e (e1, e2) -> Or_e (replace_expr a b e1, replace_expr a b e2)
    | Not_e e1 -> Not_e (replace_expr a b e1)
    | Pred_e (s1, ts) -> Pred_e (s1, replace_terms a b ts)
    | Eq_e (t1, t2) -> Eq_e (replace_term a b t1, replace_term a b t2)
    | True_e | False_e | Card_e _ -> 
        raise (Unexpected_expr_found (e, "Expand.replace_expr"))

(* matches each term with the respective item from within the element, 
 * and calls replace_expr with these two *)
let rec match_expr ts rs e = 
    assert (List.length ts = List.length rs);
    match ts with (* best to match on ts or rs? *)
    | [] -> e
 (* | t :: [] -> 
        replace_expr t (List.hd rs) e (* or case [] -> e ? *) *)
    | t :: tl -> 
        match_expr  tl (List.tl rs) ( replace_expr t (List.hd rs) e )

(* creates large conjunction of e's, calling match_expr on each and passing
 * in the terms to be replaced, one element of the set to replace, and e *)
let rec expand_forall ts set e =
    match set with 
    | [] -> raise Set_empty
    | hd :: [] -> match_expr ts hd e
    | hd :: tl -> And_e (match_expr ts hd e , expand_forall ts tl e)

let rec expand_exists ts set e =
    match set with 
    | [] -> raise Set_empty 
    | hd :: [] -> match_expr ts hd e
    | hd :: tl -> Or_e (match_expr ts hd e , expand_exists ts tl e)

(* given a tuple of vars, generate a list of terms for each var in tuple *)
let rec gen_terms vars terms =
    match vars with
    | [] -> Terms(List.rev terms)
    | hd :: tl -> gen_terms tl (Var(hd)::terms) (* reorder cons? *)

(* given pred UCHAR p and list of tuples set, construct a list of preds 
 * P(t) for each tuple t in set *)
let rec gen_preds p set preds = 
    match set with 
    | [] -> preds
    | hd :: tl -> let terms = gen_terms hd [] in
                  gen_preds p tl (Pred_e(p,terms)::preds) (*reorder cons?*)

(* do i need to keep repeating this everywhere? *) (* no.. *)
module String_map = Map.Make (String);;
(* I do need to repeat this apparently to avoid circ. dependency *)
module Str_list_set = Set.Make (struct
                                    type t = string list
                                    let compare = compare
                                end);;

(* searches for 'forall terms in set (expr)' (or 'exists'), and calls 
 * appropriate 'expand_' fn, passing in terms, set and expr *)
let rec expand_expr e sets_map = 
    match e with
    | And (e1, e2) -> 
        And_e (expand_expr e1 sets_map, expand_expr e2 sets_map)
    | Or (e1, e2) ->  
        Or_e (expand_expr e1 sets_map, expand_expr e2 sets_map)
    | Not e1 -> 
        Not_e (expand_expr e1 sets_map)
    | Forall (Terms ts, s2, e1) -> 
        let set = String_map.find s2 sets_map in 
        let set = Str_list_set.elements set in (*convert set to list*)
        expand_forall ts set (expand_expr e1 sets_map)
        (* error if s2 not in map? *)
    | Exists (Terms ts, s2, e1) -> 
        let set = String_map.find s2 sets_map in 
        let set = Str_list_set.elements set in (*convert set to list*)
        expand_exists ts set (expand_expr e1 sets_map)
    | Eq (t1, t2) ->
        Eq_e (t1, t2) (* =e *)
    | Card (p, s, eq, k) ->
        let set = String_map.find s sets_map in
        let set = Str_list_set.elements set in (*convert set to list*)
        (* let k = int_of_string ( (*should only be 1 k (an int) *)
            List.hd (List.hd (String_map.find k sets_map))) in *)
        let k_set = Str_list_set.elements (String_map.find k sets_map) in
        let k = int_of_string ( (*should only be 1 k (an int) *)
            List.hd (List.hd (k_set))) in (*THIS IS BAD NEED TO REDO *)
        let preds = gen_preds p set [] in
        Card_e (preds, eq, k)
    | Pred (s, ts) -> 
        Pred_e (s, ts)
