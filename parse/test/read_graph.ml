open Graph

module G = Persistent.Graph.Concrete (struct 
  type t = int 
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

module C = Coloring.Make(G)
module BK = Clique.Bron_Kerbosch(G)

exception File_format_error of string
exception Undefined_problem of string

let read_graph file_name = 
    let ic = open_in file_name in
    let vertices = ref [] in
    let edges = ref [] in
    try
        while true do
            let line = input_line ic in
            match Str.split (Str.regexp " ") line with
            | ["V"; v] -> 
                vertices := (int_of_string v) :: !vertices
            | ["E"; v1; v2] -> 
                edges := ((int_of_string v1),(int_of_string v2)) :: !edges 
            | ["K"; k] -> 
                ()
            | _ -> raise (File_format_error file_name) 
        done; (!vertices, !edges) 
    with 
    | End_of_file -> close_in ic;
                     (!vertices, !edges)


let read_graph file_name k = 
  print_string "reading file...\n"; flush stdout;
  let (vertices, edges) = read_graph file_name in 
  let g = List.fold_left (fun graph v -> G.add_vertex graph v) G.empty vertices in
  let g = List.fold_left (fun graph (v1, v2) -> G.add_edge graph v1 v2) g edges in
  let problem = "clique" in (* TEMP! *)
  let k_str = string_of_int k in
  match problem with
  | "col" -> (
      print_string (k_str ^ "-colouring...\n"); flush stdout;
      try 
          ignore (C.coloring g k);
          print_string (k_str ^ "-coloured!\n");
          true (* colouring found *)
      with 
          | _ -> (*Coloring.Mark(G).NoColoring *)
          print_string ("cannot be " ^ string_of_int k ^ "-coloured.\n");
          false )
  | "clique" -> (
      print_string ("finding " ^ k_str ^ "-clique...\n"); flush stdout;
      let cliques = BK.maximalcliques g in 
      (* whether a clique of length k exists or not: *)
      let result = List.exists (fun l -> List.length l >= k) cliques in
      print_string (k_str ^ "-clique " ^ (if result then "" else "not ") 
                    ^ "found\n");
      result )
  | _ -> raise (Undefined_problem problem)

(*let () = 
  let file_name = Sys.argv.(1) in
  let k = int_of_string Sys.argv.(2) in
  read_graph file_name k *)
