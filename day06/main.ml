let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; acc
let lines: string list = List.rev (read_lines (open_in "input.txt") [])

let timeRow, distanceRow = match lines with timeRow :: distanceRow :: [] -> timeRow, distanceRow | _ -> invalid_arg "bad day 6 input"
let parseRow (s: string) : int list = match String.split_on_char ' ' s with
  | _ :: rest -> rest |> List.filter (fun s -> String.length s > 0) |> List.map int_of_string
  | _ -> invalid_arg ("bad input row " ^ s)
let times, distances = (parseRow timeRow), (parseRow distanceRow)

type race = int * int (* time, record distance *)
let races = List.combine times distances

let isWinningStrategy (r: race) (i: int) : bool = match r with (time, recordDist) -> (i <= time) && (i >= 0) && ((i * (time - i)) > recordDist)
let numWinningStrategies (r: race) : int = List.init ((fst r) + 1) (fun x -> x) |> List.filter (isWinningStrategy r) |> List.length

let () = Printf.printf "part 1 solution: %d" (races |> List.map numWinningStrategies |> List.fold_left ( * ) 1)
(* part 1 solution above *)

let parseRowPart2 (s: string) : int = match (s
  |> String.fold_left (fun acc c -> if c = ' ' then acc else acc ^ (String.make 1 c)) "" (* remove spaces *)
  |> String.split_on_char ':') with
  | _ :: num :: [] -> int_of_string num
  | _ -> invalid_arg "bad input string"

let part2race = parseRowPart2 timeRow, parseRowPart2 distanceRow
let () = Printf.printf "\n\npart 2 solution: %d" (numWinningStrategies part2race)