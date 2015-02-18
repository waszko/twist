open Graph

module G = Persistent.Graph.Concrete (struct 
  type t = int 
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

module R = Rand.P(G)

let output = ref []

let rec write_vertices v =
    match v with
    | 0 -> ()
    | v -> output := ("V " ^ string_of_int v ^ "\n") :: !output;
                     write_vertices (v-1)

let write_edges v1 v2 = 
    output :=  ("E " ^ string_of_int (v1+1) ^ " " 
                             ^ string_of_int (v2+1) ^ "\n") :: !output
    (* ^ +1 needed as ocamlgraph uses node 0 *)

let gen_graph v e output_file = 
  output := [];
  Random.self_init ();
  let g = R.graph  ~loops:false ~v ~e () in (* loops? *)
  G.iter_edges write_edges g;
  write_vertices v;
  let oc = open_out output_file in
  Printf.fprintf oc "%s" (String.concat "" !output);
  close_out oc
  (*print_string !output*)

(*let () = 
  let v = int_of_string Sys.argv.(1) in
  let e = int_of_string Sys.argv.(2) in
  let output_file = Sys.argv.(3) in
  gen_graph v e output_file *)

