let data = Stdlib.open_in "input.txt"

let read_line i = try Some (input_line i) with End_of_file -> None

(* Helper function for parsing whole file into 'String list', not used here *)
let lines_from_file filename =
  let rec lines_from_file_aux i acc = match (read_line i) with
    | None -> List.rev acc
    | Some s -> lines_from_file_aux i (s :: acc) in
  lines_from_file_aux (open_in filename) []

let rec parse_input (input : Stdlib.in_channel) (current_elf : int list) (list_of_elves : int list list) : int list list =
  match
    read_line data
  with
  | None -> list_of_elves
  | Some "" -> parse_input input [] (current_elf :: list_of_elves)
  | Some cal -> parse_input input (int_of_string(cal) :: current_elf) list_of_elves

let elves : int list list = parse_input data [] []

let elf_calorie_sums : int list = List.map (List.fold_left (+) 0) elves

let max_calories : int = List.fold_left Int.max Int.min_int elf_calorie_sums

let () = Printf.printf "Max calories %d \n" max_calories

let sorted_calorie_sums : int list = List.rev (List.sort Stdlib.compare elf_calorie_sums)

let () = match sorted_calorie_sums with
| a :: b :: c :: _ -> Printf.printf "First %d Second %d Third %d Sum %d" a b c (a + b + c)
| _ -> Stdlib.print_endline "whoops"