module String_map = Map.Make (String);;

let read_instance () =
    let ic = open_in "instance.txt" in
    let sets_map = ref String_map.empty in (* map from set name to set *)
    try
        while true do
            let line = input_line ic in 
            let (name::set) = Str.split (Str.regexp " ") line in (*[]err*)
            if String_map.mem name !sets_map then
                let sets = String_map.find name !sets_map in
                sets_map := String_map.add name (set::sets) !sets_map
            else
                sets_map := String_map.add name [set] !sets_map
        done; !sets_map
    with End_of_file ->
        close_in ic;
        !sets_map (* !? *)


(* input.txt looks like:
V 1
V 2
V 3
V 4
E 1 3
E 1 4
E 3 4

from this we want 2 lists of lists:
V = [["1"];["2"];["3"];["4"]]
E = [["1";"3"];["1";"4"];["3";"4"]] *)
