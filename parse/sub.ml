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

let rec sub_expr2 e map n = 
    match e with
    | And (e1, e2) ->
        let (e1, map1, n1) = sub_expr2 e1 map n in
        let (e2, map2, n2) = sub_expr2 e2 map1 n1 in
        (And(e1,e2), map2, n2)
    | Or (e1, e2) ->
        let (e1, map1, n1) = sub_expr2 e1 map n in
        let (e2, map2, n2) = sub_expr2 e2 map1 n1 in
        (Or(e1,e2), map2, n2)
    | Not e1 ->
        let (e1, map1, n1) = sub_expr2 e1 map n in
        (Not e1, map1, n1)
    | Pred (s, ts) -> (* dont care about pred contents.. *)
        let e_str = string_of_expr e in
        if String_map.mem e_str map then 
            let sub = String_map.find e_str map in
            let p = Pred ("", Terms [Const sub]) in
            (p,map,n) 
        else 
            let n1 = n + 1 in
            let map1 = String_map.add e_str n1 map in
            let p = Pred ("", Terms [Const n1]) in
            print_string(e_str ^ " = " ^ string_of_int n1 ^ "\n");
            (p,map1,n1)  
    | _ -> (e,map,n) (* error *)

let sub_expr_call e =
    let map = String_map.empty in (* CANT GET THIS TO WORK WITH PRED MAP *)
    let n = 0 in (* highest substition so far *)
    let (exp, map, n) = sub_expr2 e map n in (* is this bad? *)
    (exp, n)
