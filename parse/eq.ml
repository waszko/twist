open Expr
open Expand
let given_sets = ref []
let sets_map = ref Io.String_map.empty

(* true iff elem is in list l *)
let in_list elem l = List.exists ((=) elem) l

(* convert terms to string list so can be compared with set *)
let rec stringlist_of_terms ts =
    match ts with
    | Terms [] -> []
    | Terms (t::tl) -> (string_of_term t) :: stringlist_of_terms (Terms tl)


(* evaluates value of equalities and existance of given predicates to 
 * {t,f}, and then distributes these to reduce ANDs and ORs *) 
let rec eq_expr e =
    match e with
    | And_e (e1, e2) -> (
        let e1 = eq_expr e1 in
        if e1 = False_e then False_e (* short circuit AND *)
        else let e2 = eq_expr e2 in
             match e1, e2 with
             | _, False_e -> False_e
             | e1, True_e -> e1
             | True_e, e2 -> e2
             | e1, e2 -> And_e (e1, e2) )
    | Or_e (e1, e2) -> (
        let e1 = eq_expr e1 in
        if e1 = True_e then True_e (* short circuit OR *)
        else let e2 = eq_expr e2 in
             match e1, e2 with
             | _, True_e -> True_e
             | e1, False_e -> e1
             | False_e, e2 -> e2
             | e1, e2 -> Or_e (e1, e2) )
    | Not_e e1 -> (
        let e1 = eq_expr e1 in
        match e1 with 
        | True_e -> False_e
        | False_e -> True_e
        | e1 -> Not_e (e1) )
    | Pred_e (s1,ts) ->
        (* test if predicate of given sets are in the sets, and convert 
         * these to {t,f} *)
        if in_list s1 !given_sets then (* if set is given *)
            let set = String_map.find s1 !sets_map in
            let ts_sl = stringlist_of_terms ts in
            if Expand.Str_list_set.mem ts_sl set then True_e else False_e
        else
            Pred_e (s1, ts)
    | Eq_e (t1,t2) -> 
        (* tests equality of predicates to convert a=b into {t,f} *)
        if Sub.cmp_term t1 t2 = 0 then True_e else False_e
    | True_e -> True_e
    | False_e -> False_e
    | Card_e _ -> e

(* sub_givens was moved inside so that its results can be short-circuited 
 * (I think..) *)

let call_eq e given_sets_arg sets_map_arg = 
    given_sets := given_sets_arg;
    sets_map := sets_map_arg;
    eq_expr e
