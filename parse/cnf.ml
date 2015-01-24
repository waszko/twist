open Expr;;

(* converts e to NNF *)
let rec nnf_expr e =
    match e with
    | Not (And(e1,e2)) -> nnf_expr ( Or (Not e1,Not e2) )
    | Not ( Or(e1,e2)) -> nnf_expr ( And(Not e1,Not e2) )
    | Not ( Not e1 ) -> nnf_expr e1
    | And (e1,e2) -> And (nnf_expr e1, nnf_expr e2)
    | Or  (e1,e2) ->  Or (nnf_expr e1, nnf_expr e2)
    | Not e1 ->  Not (nnf_expr e1) (* shouldn't act be used *)
    | Pred (s1,ts) -> Pred (s1, ts) (* RHS just e? Use _? (also below) *)
    | Card1 _ -> e
    | Card2 _ -> e
    | Forall _ | Exists _ | Eq _ | True | False -> 
        raise (Unexpected_expr_found (e, "Cnf.nnf_expr"))

(* distributes ORs inwards over ANDs, e.g: p|(q&r) -> (p|q)&(p|r) *)
let rec dist_expr e =
    match e with
    | Or (And(e1, e2), e3) -> dist_expr ( And(Or(e1,e3), Or(e2,e3)) )
    | Or (e1, And(e2, e3)) -> dist_expr ( And(Or(e1,e2), Or(e1,e3)) )
    | Or  (e1, e2) -> (* seems more complex than needed (xmas error) *)
         let e1 = dist_expr e1 in
         let e2 = dist_expr e2 in (
         match e1, e2 with 
         | And (_, _), _ -> dist_expr ( Or (e1, e2) )
         | _, And (_, _) -> dist_expr ( Or (e1, e2) )
         | _, _ -> Or (e1, e2) )
    | And (e1,e2) -> And (dist_expr e1, dist_expr e2)
    | Not e1 ->  Not e1 (* e1 must be term so recurse not needed *)
    | Pred (s1,ts) -> Pred (s1, ts) (* RHS just e? Use _? *)
    | Card1 _ -> e
    | Card2 _ -> e
    | Forall _ | Exists _ | Eq _ | True | False -> 
        raise (Unexpected_expr_found (e, "Cnf.dist_expr"))

(* tests equality of predicates to convert a=b into {t,f}, and then 
 * distributes these to reduce ANDs and ORs *) 
let rec eq_expr e =
    match e with
    | And (e1, e2) -> (
        let e1 = eq_expr e1 in
        let e2 = eq_expr e2 in
        match e1, e2 with
        | _, False -> False
        | False, _ -> False
        | e1, True -> e1
        | True, e2 -> e2
        | e1, e2 -> And (e1, e2) )
    | Or (e1, e2) -> (
        let e1 = eq_expr e1 in
        let e2 = eq_expr e2 in
        match e1, e2 with
        | _, True -> True
        | True, _ -> True
        | e1, False -> e1
        | False, e2 -> e2
        | e1, e2 -> Or (e1, e2) )
    | Not e1 -> (
        let e1 = eq_expr e1 in
        match e1 with 
        | True -> False
        | False -> True
        | e1 -> Not (e1) )
    | Pred (s1,ts) -> Pred (s1,ts)
    | Eq (t1,t2) -> if Sub.cmp_term t1 t2 = 0 then True else False
    | Forall (ts, s1, e1) -> Forall (ts, s1, eq_expr e1)
    | Exists (ts, s1, e1) -> Exists (ts, s1, eq_expr e1)
    | True -> True
    | False -> False
    | Card1 _ -> e
    | Card2 _ -> e

(* converts e into CNF *)
let cnf_expr e = dist_expr ( nnf_expr (eq_expr e) )
