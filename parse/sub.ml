open Expr;;

let cmp_term t1 t2 =
    match t1 with
    | Var s -> if t2 = Var s then 0 else -1
    | Const i -> if t2 = Const i then 0 else -1

let rec cmp_terms ts1 ts2 =
    match ts1 with
    | Terms [] -> if ts2 = Terms [] then 0 else -1
    | Terms (t1::tl1) -> 
        match ts2 with
        | Terms [] -> -1
        | Terms (t2::tl2) -> if (cmp_term t1 t2) = 0 then 
                             cmp_terms (Terms tl1) (Terms tl2) else -1

let cmp_pred p1 p2 =
    match p1 with
    | Pred (s1, ts1) -> 
        ( match p2 with
        | Pred (s2, ts2) -> if s1 = s2 then cmp_terms ts1 ts2 else -1
        | _ -> -1 )
    | _ -> -1

module Pred_map = Map.Make (struct
                              type t = expr
                              let compare = cmp_pred
                            end);;

(* NOTHING ABOVE HERE ACT USED AS I CANT GET PRED_MAP TO WORK CORRECTLY *)

module String_map = Map.Make(String);;
module Int_map = Map.Make(struct type t = int let compare = compare end)

(* is using refs ok? Or better to put back as args (more complex code) *)
let n = ref 0 (* highest substitution so far *)
let map = ref String_map.empty (* map from pred to substitution (int str)*)
                               (* CANT GET THIS TO WORK WITH PRED MAP :( *)
let rev_map = ref String_map.empty (* maps subs to preds to decode answer*)

let pbc = ref false

(* substitute pred p for var (possibly defining new substitution) *)
let sub_pred p =
    let p_str = string_of_expr p in
    if String_map.mem p_str !map then
        let sub = String_map.find p_str !map in
        Pred ("", Terms[Var sub])
    else (
        n := !n +1;
        let n_str = string_of_int !n in
        let sub = (if !pbc then "x" ^ n_str else n_str) in
        map := String_map.add p_str sub !map;
        rev_map := String_map.add sub p_str !rev_map;
        Pred ("", Terms[Var sub]) )
    
let rec sub_expr e = 
    match e with
    | And (e1, e2) -> And (sub_expr e1, sub_expr e2)
    | Or (e1, e2) -> Or (sub_expr e1, sub_expr e2)
    | Not e1 -> Not (sub_expr e1)
    | Pred _ -> sub_pred e 
    | Eq _ -> e
    | Card2 (preds, k) -> Card2 (List.rev_map sub_pred preds, k)
    | Forall _ | Exists _ | True | False | Card1 _ -> 
        raise (Unexpected_expr_found (e, "Sub.sub_expr"))

let sub_expr_call e pbc_setting =
    pbc := pbc_setting;
    let exp = sub_expr e in (* is this bad? *)
    (exp, !n, !rev_map)
