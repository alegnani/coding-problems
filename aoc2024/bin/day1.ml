let read_file file = In_channel.with_open_bin file In_channel.input_all

let file = read_file "inputs/input1"

let values = String.split_on_char '\n' file |> List.map (fun x -> String.split_on_char ' ' x |> List.filter ((<>) "") |> List.map int_of_string) |> List.filter (fun x -> not (List.is_empty x))

let l1 = List.map List.hd values |> List.sort compare
let l2 = List.map (fun x -> List.nth x 1) values |> List.sort compare


let diff = List.map2 (fun a b -> Int.abs (a - b)) l1 l2 |> List.fold_left (+) 0

let () = Printf.printf "Day 1, Part 1: %d\n" diff


let similarity = l1 |> List.map (fun x -> (List.filter ((=) x) l2 |> List.length) * x) |> List.fold_left (+) 0

let () = Printf.printf "Day 1, Part 2: %d\n" similarity
