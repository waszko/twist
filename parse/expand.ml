open Expr;;

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
    | And (e1, e2) -> And (replace_expr a b e1, replace_expr a b e2)
    | Or (e1, e2) -> Or (replace_expr a b e1, replace_expr a b e2)
    | Not e1 -> Not (replace_expr a b e1)
    | Forall (ts, s1, e1) -> Forall (ts, s1, replace_expr a b e1)
    | Exists (ts, s1, e1) -> Exists (ts, s1, replace_expr a b e1)
    | Pred (s1, ts) -> Pred (s1, replace_terms a b ts)
    | Eq (t1, t2) -> Eq (replace_term a b t1, replace_term a b t2)
    | True | False | Card1 _ | Card2 _ -> 
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
    | hd :: tl -> And (match_expr ts hd e , expand_forall ts tl e)
    (* what can i do about the warning wanting a "[]" case? *)

let rec expand_exists ts set e =
    match set with 
    | [] -> raise Set_empty 
    | hd :: [] -> match_expr ts hd e
    | hd :: tl -> Or (match_expr ts hd e , expand_exists ts tl e)

(* given a tuple of vars, generate a list of terms for each var in tuple *)
let rec gen_terms vars terms =
    match vars with
    | [] -> Terms(terms)
    | hd :: tl -> gen_terms tl (Var(hd)::terms) (* reorder cons? *)

(* given pred UCHAR p and list of tuples set, construct a list of preds 
 * P(t) for each tuple t in set *)
let rec gen_preds p set preds = 
    match set with 
    | [] -> preds
    | hd :: tl -> let terms = gen_terms hd [] in
                  gen_preds p tl (Pred(p,terms)::preds) (*reorder cons?*)

(* do i need to keep repeating this everywhere? *)
module String_map = Map.Make (String);;

(* searches for 'forall terms in set (expr)' (or 'exists'), and calls 
 * appropriate 'expand_' fn, passing in terms, set and expr *)
let rec expand_expr e sets_map = 
    match e with
    | And (e1, e2) -> 
        And (expand_expr e1 sets_map, expand_expr e2 sets_map)
    | Or (e1, e2) ->  
        Or (expand_expr e1 sets_map, expand_expr e2 sets_map)
    | Not e1 -> 
        Not (expand_expr e1 sets_map)
    | Forall (Terms ts, s2, e1) -> 
        let set = String_map.find s2 sets_map in 
        expand_forall ts set (expand_expr e1 sets_map)
        (* error if s2 not in map? *)
    | Exists (Terms ts, s2, e1) -> 
        let set = String_map.find s2 sets_map in 
        expand_exists ts set (expand_expr e1 sets_map)
    | Eq (t1, t2) ->
        Eq (t1, t2) (* =e *)
    | Card1(p, s, k) ->
        let set = String_map.find s sets_map in
        let k = int_of_string ( (*should only be 1 k (an int) *)
            List.hd (List.hd (String_map.find k sets_map))) in
        let preds = gen_preds p set [] in
        Card2(preds, k)
    | _ -> e (* is it clearer to give last 2 cases explicitly? *) 
