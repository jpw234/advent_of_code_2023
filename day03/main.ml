let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; acc
let lines: string list = read_lines (open_in "input.txt") []

type engineNumber = int * int * int * int (* value, row, startIndex, endIndex *)
type engineSymbol = char * int * int (* symbol, row, index *)

let parseRow (row: int) (s: string) : (engineNumber list * engineSymbol list) =
  let foldFn (acc: int * string * engineNumber list * engineSymbol list) (c: char) =
    match c with
    | '0' .. '9' -> (match acc with i, numString, numL, symL ->
      if i = ((String.length s) - 1) (* need to handle possibility of number ending at end of line *)
        then i + 1, "", ((int_of_string (numString ^ (String.make 1 c))), row, (i - String.length numString), i - 1) :: numL, symL
        else i + 1, (numString ^ (String.make 1 c)), numL, symL)
    | '.' -> (match acc with
      | i, "", numL, symL -> i + 1, "", numL, symL
      | i, numString, numL, symL ->
          i + 1, "", ((int_of_string numString), row, (i - (String.length numString)), i - 1) :: numL, symL)
    | sym -> (match acc with
      | i, "", numL, symL -> i + 1, "", numL, (sym, row, i) :: symL
      | i, numString, numL, symL ->
          i + 1,
          "",
          ((int_of_string numString), row, (i - (String.length numString)), i - 1) :: numL,
          (sym, row, i) :: symL)
  in match (String.fold_left foldFn (0, "", [], []) s) with _, _, numL, symL -> numL, symL

let engineNumbers, engineSymbols = (lines |> List.mapi parseRow |> List.split |> fun k -> match k with (nl, sl) -> List.flatten nl, List.flatten sl)

let isAdjacent (sym: engineSymbol) (num: engineNumber) : bool = match sym, num with
  (_, symRow, symIndex), (_, numRow, numStart, numEnd) ->
    (Int.abs (symRow - numRow)) <= 1 && (
      symIndex >= (numStart - 1) &&
      symIndex <= (numEnd + 1)
    )

let partNumberSum = engineSymbols
  |> List.map (fun sym -> List.filter (isAdjacent sym) engineNumbers) (* engineNumber list list *)
  |> List.flatten (* engineNumber list *)
  |> List.fold_left (fun acc eNum -> match eNum with value, _, _, _ -> acc + value) 0 (* int *)

let () = Printf.printf "partNumberSum = %d" partNumberSum
(* part 1 solution above *)

let gearRatioSum = engineSymbols
  |> List.filter (fun sym -> match sym with c, _, _ -> c = '*') (* filter for * symbols *)
  |> List.map (fun sym -> List.filter (isAdjacent sym) engineNumbers) (* convert symbols to lists of adjacent numbers *)
  |> List.filter (fun l -> (List.length l) = 2) (* filter to only lists that are of length 2 *)
  |> List.map (fun l -> match l with (val1, _, _, _) :: (val2, _, _, _) :: [] -> val1 * val2) (* multiply num values together *)
  |> List.fold_left (+) 0 (* sum list *)

let () = Printf.printf "\n\ngearRatioSum = %d" gearRatioSum