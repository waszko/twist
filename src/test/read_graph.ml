exception File_format_error of string

(* finds the number of edges and vertices of a given graph *)
let get_graph_size file_name = 
    let ic = open_in file_name in
    let num_vertices = ref 0 in
    let num_edges = ref 0 in
    try
        while true do
            let line = input_line ic in
            match Str.split (Str.regexp " ") line with
            | ["V"; v] -> num_vertices := !num_vertices + 1
            | ["E"; v1; v2] -> num_edges := !num_edges + 1
            | _ -> ()
        done; (!num_vertices, !num_edges) 
    with 
    | End_of_file -> close_in ic;
                     (!num_vertices, !num_edges)

(* read a dimacs_cnf file and return the # of vars and clauses *)
let get_dimacs_size file_name = 
    let ic = open_in file_name in
    let nbvars = ref 0 in
    let nbclauses = ref 0 in
    try
        while !nbvars = 0 && !nbclauses = 0 do
                let line = input_line ic in
                if line.[0] = 'p' then
                    match Str.split (Str.regexp " ") line with
                    | p::cnf::nbv::nbc::[] -> 
                        nbvars := int_of_string nbv;
                        nbclauses := int_of_string nbc
                    | _ -> raise (File_format_error "p line error")
        done; (!nbvars, !nbclauses)
    with | End_of_file -> close_in ic;
    (!nbvars, !nbclauses)

(* reads a graph file into lists of vertices and edges *)
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
