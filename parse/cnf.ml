open Sub
module S = Sub

(* converts e to NNF *)
let rec nnf_expr e =
    match e with
    | Not_s (And_s(e1,e2)) -> nnf_expr ( Or_s (Not_s e1,Not_s e2) )
    | Not_s ( Or_s(e1,e2)) -> nnf_expr ( And_s (Not_s e1,Not_s e2) )
    | Not_s ( Not_s e1 ) -> nnf_expr e1
    | And_s (e1,e2) -> And_s (nnf_expr e1, nnf_expr e2)
    | Or_s  (e1,e2) ->  Or_s (nnf_expr e1, nnf_expr e2)
    | Not_s e1 ->  Not_s (nnf_expr e1) (* shouldn't act be used *)
    | Sub_s s1 -> Sub_s s1 (* RHS just e? Use _? (also below) *)
    | Card_s _ -> e

(* distributes ORs inwards over ANDs, e.g: p|(q&r) -> (p|q)&(p|r) *)
let rec dist_expr e =
    match e with
    | Or_s (And_s(e1, e2), e3) ->
        dist_expr ( And_s(Or_s(e1,e3), Or_s(e2,e3)) )
    | Or_s (e1, And_s(e2, e3)) -> 
        dist_expr ( And_s(Or_s(e1,e2), Or_s(e1,e3)) )
    | Or_s  (e1, e2) -> (* seems more complex than needed (xmas error) *)
         let e1 = dist_expr e1 in
         let e2 = dist_expr e2 in (
         match e1, e2 with 
         | And_s (_, _), _ -> dist_expr ( Or_s (e1, e2) )
         | _, And_s (_, _) -> dist_expr ( Or_s (e1, e2) )
         | _, _ -> Or_s (e1, e2) )
    | And_s (e1,e2) -> And_s (dist_expr e1, dist_expr e2)
    | Not_s e1 ->  Not_s e1 (* e1 must be term so recurse not needed *)
    | Sub_s s1 -> Sub_s s1 (* RHS just e? Use _? *)
    | Card_s _ -> e

(* converts e into CNF *)
let cnf_expr e = dist_expr ( nnf_expr e )

(* ---- TSEITIN METHOD ---- *)
let n = ref 0 (* highest sub so far *)
let cnf = ref (Sub_s "def") (* cnf expr built up with tseitin method *)
let left = ref true (* branch to add new clauses on to *)
let pbc = ref false (* are pbc being used? *)
let rev_map = ref Sub.String_map.empty (* map of subs to preds from Sub *)

(* extends cnf with new clauses, adding to alternate branches to reduce
 * the depth of the tree *)
let add_clauses c =
    if !left then ( left := false; cnf := And_s (c, !cnf) )
             else ( left := true; cnf := And_s (!cnf, c) )

(* generate a new variable to use as the result of a Tseitin operation *)
let gen_variable _ =
    n := !n + 1;
    let n_str = string_of_int !n in
    let v = Sub_s n_str in 
    (* add to reverse predicate map with blank value: *)
    let sub = (if !pbc then "x" ^ n_str else n_str) in
    rev_map := Sub.String_map.add sub "" !rev_map;
    v

(* Tseitin transform for converting to CNF in linear time *)
(* creates many more variables, possibly resulting in slower SAT-solving *)
let rec tseitin_expr e =
    match e with
    | And_s (e1, e2) ->
        let v1 = tseitin_expr e1 in (* must be a variable (v) *)
        let v2 = tseitin_expr e2 in
        let v3 = gen_variable () in
        (* (~v1 | ~v2 | v3) & (v1 | ~v3) & (v2 | ~v3) *)
        add_clauses (And_s(Or_s(Or_s(Not_s v1, Not_s v2), v3),
                     And_s(Or_s(v1, Not_s v3), Or_s(v2, Not_s v3))) );
        v3
    | Or_s (e1, e2) ->
        let v1 = tseitin_expr e1 in
        let v2 = tseitin_expr e2 in
        let v3 = gen_variable () in
        (* (v1 | v2 | ~v3) & (~v1 | v3) & (~v2 | v3) *)
        add_clauses (And_s(Or_s(Or_s(v1,v2),Not_s v3),
                     And_s(Or_s(Not_s v1,v3),Or_s(Not_s v2,v3))) );
        v3
    | Not_s v1 -> (* must be a predicate *)
        let v2 = gen_variable () in
        (* (~v1 | ~v2) & (v1 | v2) *)
        add_clauses ( And_s(Or_s(Not_s v1, Not_s v2), Or_s(v1, v2)) ); 
        v2
    | Sub_s s1 -> Sub_s s1
    | Card_s _ -> e (* ? *)

let tseitin_cnf_expr e nbvars prev_rev_map pbc_value = 
    rev_map := prev_rev_map;
    pbc := pbc_value;
    n := nbvars;
    cnf := gen_variable (); (* begin with blank var? *)
    let p0 = tseitin_expr ( nnf_expr e ) in
    (And_s (!cnf, p0), !n, !rev_map)
