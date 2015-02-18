open Expand

(* tests equality of predicates to convert a=b into {t,f}, and then 
 * distributes these to reduce ANDs and ORs *) 
let rec eq_expr e =
    match e with
    | And_e (e1, e2) -> (
        let e1 = eq_expr e1 in
        let e2 = eq_expr e2 in
        match e1, e2 with
        | _, False_e -> False_e
        | False_e, _ -> False_e
        | e1, True_e -> e1
        | True_e, e2 -> e2
        | e1, e2 -> And_e (e1, e2) )
    | Or_e (e1, e2) -> (
        let e1 = eq_expr e1 in
        let e2 = eq_expr e2 in
        match e1, e2 with
        | _, True_e -> True_e
        | True_e, _ -> True_e
        | e1, False_e -> e1
        | False_e, e2 -> e2
        | e1, e2 -> Or_e (e1, e2) )
    | Not_e e1 -> (
        let e1 = eq_expr e1 in
        match e1 with 
        | True_e -> False_e
        | False_e -> True_e
        | e1 -> Not_e (e1) )
    | Pred_e (s1,ts) -> Pred_e (s1,ts)
    | Eq_e (t1,t2) -> if Sub.cmp_term t1 t2 = 0 then True_e else False_e
    | True_e -> True_e
    | False_e -> False_e
    | Card_e _ -> e
