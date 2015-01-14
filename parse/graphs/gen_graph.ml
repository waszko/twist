module Pair_set = Set.Make(struct
                              type t = (int*int)
                              let compare = compare
                           end)

let rec write_edges oc num_nodes edges edges_left =
    match edges_left with
    | 0 -> ()
    | _ -> let node1 = (Random.int num_nodes) + 1 in
           let node2 = (Random.int num_nodes) + 1 in
           if Pair_set.mem (node1, node2) edges then (
               write_edges oc num_nodes edges edges_left )
           else (
               Printf.fprintf oc "E %d %d\n" node1 node2;
               let edges = Pair_set.add (node1, node2) edges in
               let edges = Pair_set.add (node2, node1) edges in (*undir*)
               write_edges oc num_nodes edges (edges_left -1) )

let () =
    let num_nodes = int_of_string Sys.argv.(1) in
    let num_edges = int_of_string Sys.argv.(2) in
    let oc = open_out Sys.argv.(3) in

    for node = 1 to num_nodes do
        Printf.fprintf oc "V %d\n" node
    done;

    Random.self_init();
    let edges = Pair_set.empty in
    write_edges oc num_nodes edges num_edges
