(* decodes graphs from dimacs graph format to form readable by my parser *)
let () =
    let ic = open_in Sys.argv.(1) in
    let oc = open_out Sys.argv.(2) in
    try
        while true do
            let line = input_line ic in
            let first_char = line.[0] in
            match first_char with 
            | 'p' -> let split = Str.split (Str.regexp " ") line in
                     let nbnodes = int_of_string (List.nth split 2) in
                     for node = 1 to nbnodes do
                         Printf.fprintf oc "V %d\n" node
                     done
            | 'e' -> let length = String.length line in
                     let remainder = String.sub line 1 (length-1) in
                     Printf.fprintf oc "E%s\n" remainder
            | _ -> ()(* skip comments etc *)
        done
    with End_of_file ->
        close_in ic;
        close_out oc
