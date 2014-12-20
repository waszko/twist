open Expr;;

(* temp hard coded *)
let vertices = [["1"];["2"];["3"];["4"]];; (* this a list of lists? *)
let edges = [["1";"3"];["1";"4"];["3";"4"]];; (* & should these be ints? *)
(*let vertices = [["1"];["2"];["3"];["4"];["5"];["6"];["7"]];; 
let edges = [["1";"2"];["2";"3"];["3";"4"];["4";"5"];["5";"6"];["6";"1"];
             ["1";"7"];["2";"7"];["3";"7"];["4";"7"];["5";"7"];["6";"7"]];;
let vertices = [["1"];["2"];["3"]];;
let edges = [["1";"2"];["2";"3"]];; *)

let replace_term a b t = 
    match t with
    | Const c -> Const c
    | Var v -> if (Var v) = a then Var b else Var v (* if v = a then *)

let rec replace_terms a b ts =
    match ts with
 (* | Term t -> Term (replace_term a b t) *)
    | Terms ts -> Terms ( List.map (replace_term a b) ts )

let rec replace_expr a b e =
    match e with
    | And (e1,e2) -> And (replace_expr a b e1, replace_expr a b e2)
    | Or  (e1,e2) ->  Or (replace_expr a b e1, replace_expr a b e2)
    | Not e1 ->  Not (replace_expr a b e1)
    | Forall (ts,s1,e1) -> Forall (ts, s1, replace_expr a b e1)
    | Exists (ts,s1,e1) -> Exists (ts, s1, replace_expr a b e1)
    | Pred (s1,ts) -> Pred (s1, replace_terms a b ts)
    | Eq (t1,t2) -> Eq (replace_term a b t1, replace_term a b t2)

let rec match_expr ts rs e = (* ts and rs should be same length *)
                             (* which is best to match on ? *)
    match ts with
    | t :: [] -> replace_expr t (List.hd rs) e (* or case [] -> e ? *)
    | t :: tl -> match_expr  tl (List.tl rs) ( 
                               replace_expr t (List.hd rs) e )

let rec expand_forall ts l e =
    match l with 
    | hd :: [] -> match_expr ts hd e
    | hd :: tl -> And (match_expr ts hd e , expand_forall ts tl e)
    (* what can i do about the warning wanting a "[]" case? *)

let rec expand_exists ts l e =
    match l with 
    | hd :: [] -> match_expr ts hd e
    | hd :: tl -> Or (match_expr ts hd e , expand_exists ts tl e)

let rec expand_expr e = 
    match e with
    | And (e1,e2) -> And (expand_expr e1, expand_expr e2)
    | Or  (e1,e2) ->  Or (expand_expr e1, expand_expr e2)
    | Not e1 -> Not (expand_expr e1)
    | Forall (Terms ts, s2, e1) -> 
        if s2 = "V" then expand_forall ts vertices (expand_expr e1)
        else if s2 = "E" then expand_forall ts edges (expand_expr e1)
     (* else if s2 = "B" then expand_forall s1 edges2 (expand_expr e1) *)
        else expand_expr e
    | Exists (Terms ts, s2, e1) -> 
        if s2 = "V" then expand_exists ts vertices (expand_expr e1)
        else if s2 = "E" then expand_exists ts edges (expand_expr e1)
     (* else if s2 = "B" then expand_exists s1 edges2 (expand_expr e1) *)
        else expand_expr e
    | _ -> e (* is it clearer to give last 2 cases explicitly? *) 
