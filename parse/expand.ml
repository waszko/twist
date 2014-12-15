open Expr;;

(* temp hard coded *)
let vertices = ["1";"2";"3";"4"];; (* should these be ints? *)
let edges = [("1","3");("1","4");("3","4")];;

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
    | Quant (s1,s2,e1) -> Quant (s1, s2, replace_expr a b e1)
    | Pred (s1,ts) -> Pred (s1, replace_terms a b ts)
    | Eq (t1,t2) -> Eq (replace_term a b t1, replace_term a b t2)

let rec expand_forall s l e =
    match l with 
    | hd :: [] -> replace_expr s hd e
    | hd :: tl -> And (replace_expr s hd e , expand_forall s tl e)
    (* what can i do about the warning wanting a "[]" case? *)

let expand_expr e = 
    match e with
    | Quant (s1, s2, e1) -> if s2 = "V" then expand_forall s1 vertices e1
                                        else e
    | _ -> e
