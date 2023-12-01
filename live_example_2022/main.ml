let data = Stdlib.open_in "input.txt"

let read_line i = try Some (input_line i) with End_of_file -> None

let rec parse_input (input : Stdlib.in_channel) (elf : int list) (elves : int list list) : int list list =
  match
    read_line input
  with
  | None -> elves
  | Some "" -> parse_input input [] (elf :: elves)
  | Some str -> parse_input input (int_of_string(str) :: elf) elves

let elves : int list list = parse_input data [] []

(* int list list. first steP: convert from int list list -> int list*)

let elf_calorie_sums : int list = List.map (List.fold_left (+) 0) elves

let max_calories : int = List.fold_left Int.max 0 elf_calorie_sums

let () = Printf.printf "Max calories %d \n" max_calories

let sorted_calorie_sums : int list = List.rev (List.sort Stdlib.compare elf_calorie_sums)

let () = match sorted_calorie_sums with
| a :: b :: c :: _ -> Printf.printf "First %d Second %d Third %d Sum %d" a b c (a + b + c)
| _ -> Printf.printf "Ouch"