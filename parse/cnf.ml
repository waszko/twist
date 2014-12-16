open Expr;;

let rec nnf_expr e =
    match e with
    | Not (And(e1,e2)) -> nnf_expr ( Or(Not(nnf_expr e1),Not(nnf_expr e2)))
    | Not ( Or(e1,e2)) -> nnf_expr (And(Not(nnf_expr e1),Not(nnf_expr e2)))
    | Not ( Not e1 ) -> nnf_expr e1
    | And (e1,e2) -> And (nnf_expr e1, nnf_expr e2)
    | Or  (e1,e2) ->  Or (nnf_expr e1, nnf_expr e2)
    | Not e1 ->  Not (nnf_expr e1) (* shouldn't act be used *)
    | Pred (s1,ts) -> Pred (s1, ts) (* RHS just e? Use _? (also below) *)
    | Eq (t1,t2) -> Eq (t1, t2)
    (* these cases should not appear as after expansion? *)
    | Forall (s1,s2,e1) -> Forall (s1, s2, nnf_expr e1) 
    | Exists (s1,s2,e1) -> Exists (s1, s2, nnf_expr e1)

let rec dist_expr e =
    match e with
    | Or (And(e1, e2), e3) -> dist_expr ( 
        And(Or(dist_expr e1,dist_expr e3),Or(dist_expr e2,dist_expr e3)) )
    | Or (e1, And(e2, e3)) -> dist_expr (
        And(Or(dist_expr e1,dist_expr e2),Or(dist_expr e1,dist_expr e3)) )
    | And (e1,e2) -> And (dist_expr e1, dist_expr e2)
    | Or  (e1,e2) ->  Or (dist_expr e1, dist_expr e2)
    | Not e1 ->  Not (dist_expr e1)
    | Pred (s1,ts) -> Pred (s1, ts) (* RHS just e? Use _? (also below) *)
    | Eq (t1,t2) -> Eq (t1, t2)
    (* these cases should not appear as after expansion? *)
    | Forall (s1,s2,e1) -> Forall (s1, s2, dist_expr e1)
    | Exists (s1,s2,e1) -> Exists (s1, s2, dist_expr e1)

let cnf_expr e = dist_expr ( nnf_expr e ) 
