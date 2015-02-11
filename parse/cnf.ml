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
 (* | Forall _ | Exists _ | Eq _ | True | False -> 
        raise (Unexpected_expr_found (e, "Cnf.nnf_expr")) *)
    | Forall (ts, s1, e1) -> Forall (ts, s1, nnf_expr e1)
    | Exists (ts, s1, e1) -> Exists (ts, s1, nnf_expr e1)
    | _ -> e

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
 (* | Forall _ | Exists _ | Eq _ | True | False -> 
        raise (Unexpected_expr_found (e, "Cnf.dist_expr")) *)
    | Forall (ts, s1, e1) -> Forall (ts, s1, dist_expr e1)
    | Exists (ts, s1, e1) -> Exists (ts, s1, dist_expr e1)
    | _ -> e

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

(* ---- TSEITIN METHOD ---- *)
let n = ref 0 (* highest sub so far *)
let cnf = ref True (* cnf expression built up with tseitin method *)
let left = ref true (* branch to add new clauses on to *)
let pbc = ref false (* are pbc being used? *)
let rev_map = ref Sub.String_map.empty (* map of subs to preds from Sub *)

(* extends cnf with new clauses, adding to alternate branches to reduce
 * the depth of the tree *)
let add_clauses c =
    if !left then ( left := false; cnf := And (c, !cnf) )
             else ( left := true; cnf := And (!cnf, c) )

(* generate a new variable to use as the result of a Tseitin operation *)
let gen_variable _ =
    n := !n + 1;
    let n_str = string_of_int !n in
    let v = Pred("", Terms[Var (n_str)] ) in 
    (* add to reverse predicate map with blank value: *)
    let sub = (if !pbc then "x" ^ n_str else n_str) in
    rev_map := Sub.String_map.add sub "" !rev_map;
    v

(* Tseitin transform for converting to CNF in linear time *)
(* creates many more variables, possibly resulting in slower SAT-solving *)
let rec tseitin_expr e =
    match e with
    | And (e1, e2) ->
        let v1 = tseitin_expr e1 in (* must be a variable (v) *)
        let v2 = tseitin_expr e2 in
        let v3 = gen_variable () in
        (* (~v1 | ~v2 | v3) & (v1 | ~v3) & (v2 | ~v3) *)
        add_clauses (And(Or(Or(Not v1, Not v2), v3),
                     And(Or(v1, Not v3), Or(v2, Not v3))) );
        v3
    | Or (e1, e2) ->
        let v1 = tseitin_expr e1 in
        let v2 = tseitin_expr e2 in
        let v3 = gen_variable () in
        (* (v1 | v2 | ~v3) & (~v1 | v3) & (~v2 | v3) *)
        add_clauses 
            (And(Or(Or(v1,v2),Not v3),And(Or(Not v1,v3),Or(Not v2,v3))) );
        v3
    | Not v1 -> (* must be a predicate *)
        let v2 = gen_variable () in
        (* (~v1 | ~v2) & (v1 | v2) *)
        add_clauses ( And(Or(Not v1, Not v2), Or(v1, v2)) ); 
        v2
    | Pred (s1,ts) -> Pred (s1,ts)
    | Card1 _ -> e (* ? *)
    | Card2 _ -> e
    | Forall _ | Exists _ | Eq _ | True | False -> 
        raise (Unexpected_expr_found (e, "Cnf.dist_expr")) 

let tseitin_cnf_expr e nbvars prev_rev_map pbc_value = 
    rev_map := prev_rev_map;
    pbc := pbc_value;
    n := nbvars;
    cnf := gen_variable (); (* begin with blank var? *)
    let p0 = tseitin_expr ( nnf_expr (eq_expr e) ) in
    (And (!cnf, p0), !n, !rev_map)
