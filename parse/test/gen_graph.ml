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

let write_k k = output := ("K " ^ string_of_int k ^ "\n") :: !output

(* Generate a graph of size |V|, |E| with parameter K *)
let gen_graph v e k output_file = 
  output := [];
  Random.self_init ();
  let g = R.graph  ~loops:false ~v ~e () in (* loops? *)
  G.iter_edges write_edges g;
  write_vertices v;
  write_k k;
  let oc = open_out output_file in
  Printf.fprintf oc "%s" (String.concat "" !output);
  close_out oc
