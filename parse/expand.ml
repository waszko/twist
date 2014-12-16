open Expr;;

(* temp hard coded *)
let vertices = ["1";"2";"3";"4"];; (* should these be ints? *)
let edges1 = ["1";"1";"3"];;
let edges2 = ["3";"4";"4"];;

let replace_term a b t = 
    match t with
    | Const c -> Const c
    | Var v -> if v = a then Var b else Var v

let rec replace_terms a b ts =
    match ts with
    | Term t -> Term (replace_term a b t)
    | Terms (ts,t) -> Terms (replace_terms a b ts, replace_term a b t)

let rec replace_expr a b e =
    match e with
    | And (e1,e2) -> And (replace_expr a b e1, replace_expr a b e2)
    | Or  (e1,e2) ->  Or (replace_expr a b e1, replace_expr a b e2)
    | Not e1 ->  Not (replace_expr a b e1)
    | Forall (s1,s2,e1) -> Forall (s1, s2, replace_expr a b e1)
    | Exists (s1,s2,e1) -> Exists (s1, s2, replace_expr a b e1)
    | Pred (s1,ts) -> Pred (s1, replace_terms a b ts)
    | Eq (t1,t2) -> Eq (replace_term a b t1, replace_term a b t2)

let rec expand_forall s l e =
    match l with 
    | hd :: [] -> replace_expr s hd e
    | hd :: tl -> And (replace_expr s hd e , expand_forall s tl e)
    (* what can i do about the warning wanting a "[]" case? *)

let rec expand_exists s l e =
    match l with 
    | hd :: [] -> replace_expr s hd e
    | hd :: tl -> Or (replace_expr s hd e , expand_exists s tl e)

let rec expand_expr e = 
    match e with
    | And (e1,e2) -> And (expand_expr e1, expand_expr e2)
    | Or  (e1,e2) ->  Or (expand_expr e1, expand_expr e2)
    | Not e1 -> Not (expand_expr e1)
    | Forall (s1, s2, e1) -> 
        if s2 = "V" then expand_forall s1 vertices (expand_expr e1)
        else if s2 = "A" then expand_forall s1 edges1 (expand_expr e1)
        else if s2 = "B" then expand_forall s1 edges2 (expand_expr e1)
        else expand_expr e
    | Exists (s1, s2, e1) -> 
        if s2 = "V" then expand_exists s1 vertices (expand_expr e1)
        else if s2 = "A" then expand_exists s1 edges1 (expand_expr e1)
        else if s2 = "B" then expand_exists s1 edges2 (expand_expr e1)
        else expand_expr e
    | _ -> e (* is it clearer to give last 2 cases explicitly? *) 
