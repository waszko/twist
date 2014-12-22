open Expr;;

let replace_term a b t = 
    match t with
    | Const c -> Const c
    | Var v -> if (Var v) = a then Var b else Var v (* if v = a then *)

let rec replace_terms a b ts =
    match ts with
 (* | Term t -> Term (replace_term a b t) *)
    | Terms ts -> Terms ( List.map (replace_term a b) ts )

let rec replace_expr a b e =
    match e with
    | And (e1,e2) -> And (replace_expr a b e1, replace_expr a b e2)
    | Or  (e1,e2) ->  Or (replace_expr a b e1, replace_expr a b e2)
    | Not e1 ->  Not (replace_expr a b e1)
    | Forall (ts,s1,e1) -> Forall (ts, s1, replace_expr a b e1)
    | Exists (ts,s1,e1) -> Exists (ts, s1, replace_expr a b e1)
    | Pred (s1,ts) -> Pred (s1, replace_terms a b ts)
    | Eq (t1,t2) -> Eq (replace_term a b t1, replace_term a b t2)

let rec match_expr ts rs e = (* ts and rs should be same length *)
                             (* which is best to match on ? *)
    match ts with
    | t :: [] -> replace_expr t (List.hd rs) e (* or case [] -> e ? *)
    | t :: tl -> match_expr  tl (List.tl rs) ( 
                               replace_expr t (List.hd rs) e )

let rec expand_forall ts l e =
    match l with 
    | hd :: [] -> match_expr ts hd e
    | hd :: tl -> And (match_expr ts hd e , expand_forall ts tl e)
    (* what can i do about the warning wanting a "[]" case? *)

let rec expand_exists ts l e =
    match l with 
    | hd :: [] -> match_expr ts hd e
    | hd :: tl -> Or (match_expr ts hd e , expand_exists ts tl e)

(* do i need to keep repeating this everywhere? *)
module String_map = Map.Make (String);;

let rec expand_expr e sets_map = 
    match e with
    | And (e1,e2) -> And (expand_expr e1 sets_map, expand_expr e2 sets_map)
    | Or  (e1,e2) ->  Or (expand_expr e1 sets_map, expand_expr e2 sets_map)
    | Not e1 -> Not (expand_expr e1 sets_map)
    | Forall (Terms ts, s2, e1) -> 
        let set = String_map.find s2 sets_map in 
        expand_forall ts set (expand_expr e1 sets_map)
        (* error if s2 not in map? *)
    | Exists (Terms ts, s2, e1) -> 
        let set = String_map.find s2 sets_map in 
        expand_exists ts set (expand_expr e1 sets_map)
    | _ -> e (* is it clearer to give last 2 cases explicitly? *) 
