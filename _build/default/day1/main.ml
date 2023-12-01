let data = open_in "input.txt"

let read_line i = try Some (input_line i) with End_of_file -> None

let is_digit c = match c with '0' .. '9' -> true | _ -> false

let first_digit s =
  let keep_one_digit acc c =
    match acc with
    | ' ' -> 
      (match is_digit c with
      | true -> c
      | false -> ' ')
    | d -> d
  in String.fold_left keep_one_digit ' ' s

let last_digit s =
  let keep_one_digit c acc =
    match acc with
    | ' ' ->
      (match is_digit c with
      | true -> c
      | false -> ' ')
    | d -> d
  in String.fold_right keep_one_digit s ' '

let rec parse_input (input: Stdlib.in_channel) (output: int list) : int list =
  match read_line input with
  | None -> output
  | Some str -> parse_input input (int_of_string ((Char.escaped (first_digit str)) ^ (Char.escaped (last_digit str))) :: output)

let calibration_values = parse_input data []

let calibration_values_sum = List.fold_left (+) 0 calibration_values

let () = Printf.printf "Calibration values sum: %d" calibration_values_sum
(* part 1 solution *)