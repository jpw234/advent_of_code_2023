let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

let histories = lines |> List.map (String.split_on_char ' ') |> List.map (List.map int_of_string) |> List.map List.rev

let rec historyToDiffTree (h: int list list) : int list list =
  let firstList :: _ = h in
  let rec generateDiffs (l: int list) : int list = match l with
    | a :: b :: rest -> (a - b) :: (generateDiffs (b :: rest))
    | last :: [] -> []
    | [] -> invalid_arg "can't input empty list to generateDiffs" in
  let nextDiff = generateDiffs firstList in
  if List.for_all ((=) 0) nextDiff
    then nextDiff :: h
    else historyToDiffTree (nextDiff :: h)

let nextVal (combiner: int -> int -> int ) (diffTree: int list list) : int =
  let zeroList :: rest = diffTree in
  let rec generator (l: int list list) : int = match l with
    | (h1 :: _) :: (h2 :: t2) :: rest -> generator (((combiner h2 h1) :: h2 :: t2) :: rest)
    | (h1 :: _) :: [] -> h1
    | _ -> invalid_arg "bad input to generator" in
  generator ((0 :: zeroList) :: rest)

let () = Printf.printf "sum of extrapolated values: %d"
  (histories |> List.map (fun l -> [l]) |> List.map historyToDiffTree |> List.map (nextVal (+)) |> List.fold_left (+) 0)
(* part 1 solution above *)

let () = Printf.printf "\n\nsum of extrapolated prev values: %d"
  (histories |> List.map (fun l -> [l]) |> List.map historyToDiffTree |> List.map (List.map List.rev) |> List.map (nextVal (-)) |> List.fold_left (+) 0)