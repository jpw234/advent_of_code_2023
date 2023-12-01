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

let last_digit s = (* I wanted to just do String.reverse and call first_digit but I don't see that fn in the standard library? *)
  let keep_one_digit c acc =
    match acc with
    | ' ' ->
      (match is_digit c with
      | true -> c
      | false -> ' ')
    | d -> d
  in String.fold_right keep_one_digit s ' '

let rec parse_input_part1 (input: Stdlib.in_channel) (output: int list) : int list =
  match read_line input with
  | None -> output
  | Some str -> parse_input_part1 input (int_of_string ((Char.escaped (first_digit str)) ^ (Char.escaped (last_digit str))) :: output)

let calibration_values = parse_input_part1 data []

let calibration_values_sum = List.fold_left (+) 0 calibration_values

let () = Printf.printf "Calibration values sum: %d \n" calibration_values_sum

let () = close_in data
(* part 1 solution above *)

let patterns = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "zero";
  "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"]

let int_value_from_pattern p = match p with
  | "zero" | "0" -> 0
  | "one" | "1" -> 1
  | "two" | "2" -> 2
  | "three" | "3" -> 3
  | "four" | "4" -> 4
  | "five" | "5" -> 5
  | "six" | "6" -> 6
  | "seven" | "7" -> 7
  | "eight" | "8" -> 8
  | "nine" | "9" -> 9
  | _ -> invalid_arg "not a number"

let find_first_pattern str =
  let pattern_check acc pattern = match acc with 
  | (old_pattern, old_first_occurrence) -> let first_occurrence = try Str.search_forward (Str.regexp_string pattern) str 0 with Not_found -> Int.max_int in
    if first_occurrence < old_first_occurrence then (pattern, first_occurrence) else (old_pattern, old_first_occurrence) in
  List.fold_left pattern_check ("", Int.max_int) patterns

let find_last_pattern str =
  let pattern_check acc pattern = match acc with
  | (old_pattern, old_last_occurrence) -> let last_occurrence = try Str.search_backward (Str.regexp_string pattern) str (String.length str) with Not_found -> Int.min_int in
    if last_occurrence > old_last_occurrence then (pattern, last_occurrence) else (old_pattern, old_last_occurrence) in
  List.fold_left pattern_check ("", Int.min_int) patterns

let data = open_in "input.txt"

let rec parse_input_part2 (input: Stdlib.in_channel) (output: int list) : int list =
  match read_line input with
  | None -> output
  | Some str -> parse_input_part2 input (int_value_from_pattern (fst (find_first_pattern str)) * 10 + int_value_from_pattern (fst (find_last_pattern str)) :: output)


let part2_calibration_values = parse_input_part2 data []

let part2_calibration_values_sum = List.fold_left (+) 0 part2_calibration_values

let () = Printf.printf "Part 2 calibration values sum: %d" part2_calibration_values_sum

let () = close_in data
(* part 2 solution above *)
