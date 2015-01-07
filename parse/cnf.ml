open Expr;;

let rec nnf_expr e =
    match e with
    | Not (And(e1,e2)) -> nnf_expr ( Or (Not e1,Not e2) )
    | Not ( Or(e1,e2)) -> nnf_expr ( And(Not e1,Not e2) )
    | Not ( Not e1 ) -> nnf_expr e1
    | And (e1,e2) -> And (nnf_expr e1, nnf_expr e2)
    | Or  (e1,e2) ->  Or (nnf_expr e1, nnf_expr e2)
    | Not e1 ->  Not (nnf_expr e1) (* shouldn't act be used *)
    | Pred (s1,ts) -> Pred (s1, ts) (* RHS just e? Use _? (also below) *)
    | Eq (t1,t2) -> Eq (t1, t2)
    (* these cases should not appear as after expansion? *)
    | Forall (ts,s1,e1) -> Forall (ts, s1, nnf_expr e1) 
    | Exists (ts,s1,e1) -> Exists (ts, s1, nnf_expr e1)
    | True | False -> raise (Unexpected_expr_found (e, "Cnf.nnf_expr"))

let rec dist_expr e =
    match e with
    | Or (And(e1, e2), e3) -> dist_expr ( 
        And(dist_expr(Or(e1,e3)),dist_expr(Or(e2,e3))) ) (* recurse dist_expr on more parts? *)
    | Or (e1, And(e2, e3)) -> dist_expr (
        And(dist_expr(Or(e1,e2)),dist_expr(Or(e1,e3))) )
    | Or  (e1,e2) -> (* seems more complex than it should be (xmas error)*)
         let e1 = dist_expr e1 in
         let e2 = dist_expr e2 in (
         match e1, e2 with 
         | And (_, _), _ -> dist_expr ( Or (e1, e2) )
         | _, And (_, _) -> dist_expr ( Or (e1, e2) )
         | _, _ -> Or (e1, e2) )
    | And (e1,e2) -> And (dist_expr e1, dist_expr e2)
    | Not e1 ->  Not (dist_expr e1) (* e1 must be term? So not needed? *)
    | Pred (s1,ts) -> Pred (s1, ts) (* RHS just e? Use _? (also below) *)
    | Eq (t1,t2) -> Eq (t1, t2)
    (* these cases should not appear as after expansion? *)
    | Forall (ts,s1,e1) -> Forall (ts, s1, dist_expr e1)
    | Exists (ts,s1,e1) -> Exists (ts, s1, dist_expr e1)
    | True | False -> raise (Unexpected_expr_found (e, "Cnf.dist_expr"))

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

let cnf_expr e = dist_expr ( nnf_expr (eq_expr e) )
