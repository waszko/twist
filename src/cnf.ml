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
let left = ref true (* branch to add new clauses on to *)
let pbc = ref false (* are pbc being used? *)
let rev_map = ref Sub.String_map.empty (* map of subs to preds from Sub *)
let clause_list = ref [] (* list of tseitsin generated clauses *)

(* adds clause c to list of tseitsin generated clauses *)
let add_clauses c =
    clause_list := c :: !clause_list

(* generate a new variable to use as the result of a Tseitin operation *)
let gen_variable _ =
    n := !n + 1;
    let n_str = string_of_int !n in
    let sub = (if !pbc then "x" ^ n_str else n_str) in
    let v = Sub_s sub in 
    (* add to reverse predicate map with blank value: *)
    rev_map := Sub.String_map.add sub "" !rev_map;
    v

(* if arg is a predicate returns it, else gens a new var *)
let get_variable e =
    match e with
    | Sub_s s1 -> Sub_s s1
    | _ -> gen_variable () 

(* Tseitin transform for converting to CNF in linear time *)
(* creates many more variables, possibly resulting in slower SAT-solving *)

let rec tseitin_expr e v3 = (* v3 = result of gate var, passed down *)
    match e with
    | And_s (e1, e2) ->
        let v1 = get_variable e1 in
        let v2 = get_variable e2 in
        (* (~v1 | ~v2 | v3) & (v1 | ~v3) & (v2 | ~v3) *)
        add_clauses (And_s(Or_s(Or_s(Not_s v1, Not_s v2), v3),
                     And_s(Or_s(v1, Not_s v3), Or_s(v2, Not_s v3))) );
        tseitin_expr e1 v1;
        tseitin_expr e2 v2
    | Or_s (e1, e2) ->
        let v1 = get_variable e1 in
        let v2 = get_variable e2 in
        (* (v1 | v2 | ~v3) & (~v1 | v3) & (~v2 | v3) *)
        add_clauses (And_s(Or_s(Or_s(v1,v2),Not_s v3),
                     And_s(Or_s(Not_s v1,v3),Or_s(Not_s v2,v3))) );
        tseitin_expr e1 v1;
        tseitin_expr e2 v2
    | Not_s v1 -> (* must be a predicate *)
        let v2 = v3 in
        (* (~v1 | ~v2) & (v1 | v2) *)
        add_clauses ( And_s(Or_s(Not_s v1, Not_s v2), Or_s(v1, v2)) )
    | Sub_s s1 -> ()
    | Card_s _ -> 
        add_clauses e (* treat card constraint as a clause *)
        (* does this work? add_clauses And(e, v3) ? *)

(* converts list of clauses into balanced binary tree, by repeatedly
 * pairing elements of the list, then sending the pair to the end of the
 * list, until the list is of size 1 *)
let rec assemble_tree clause_list paired =
    match clause_list with
    | [] -> ( match paired with
              | hd::[] -> hd
              | _ -> assemble_tree paired []  )
    | c1::[] -> assemble_tree [] (c1 :: paired)
    | c1::c2::tl -> assemble_tree tl (And_s(c1,c2) :: paired) 

let tseitin_cnf_expr e nbvars prev_rev_map pbc_value = 
    rev_map := prev_rev_map;
    pbc := pbc_value;
    n := nbvars;
    let result_var = gen_variable () in
    clause_list := [result_var];
    tseitin_expr ( nnf_expr e) result_var;
    let cnf = assemble_tree !clause_list [] in
    (cnf, !n, !rev_map)
