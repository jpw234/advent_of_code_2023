let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; acc
let lines: string list = List.rev (read_lines (open_in "input.txt") [])

type range = int * int * int (* destSart, sourceStart, rangeLength *)
type map = range list

let applyRange (i: int) (r: range) : int option = match r with
  (d, s, len) ->
    if (i >= s) && (i <= (s + len))
      then Some (d + (i - s))
      else None

let applyMap (i: int) (m: map) : int = match List.find_map (applyRange i) m with Some o -> o | None -> i

let parseSeedLine (s: string) : int list = List.nth (String.split_on_char ':' s) 1 |> String.trim |> String.split_on_char ' ' |> List.map int_of_string

let seeds, mapLines = match lines with seedLine :: "" :: rest -> parseSeedLine seedLine, rest

let maps =
  let parseMapLines (acc: map * map list) s = match (acc, s) with
  | (currMap, maps), "" -> [], (List.rev currMap) :: maps
  | (currMap, maps), line ->
    if (String.contains line ':') (* if there's a colon, it's a map name line, ignore it *)
      then currMap, maps
      else (match (line |> String.split_on_char ' ' |> List.map int_of_string) with
      | a :: b :: c :: [] -> (a, b, c) :: currMap, maps
      | _ -> invalid_arg "bad range")
  in match (List.fold_left parseMapLines ([], []) mapLines) with (map, maps) -> List.rev ((List.rev map) :: maps)

let locations = List.map (fun seed -> List.fold_left applyMap seed maps) seeds
let () = Printf.printf "lowest location # = %d" (List.fold_left Int.min Int.max_int locations)
(* solution to part 1 above *)

type numRange = int * int (* start, length *)

let applyRangeToNumRange (nr: numRange) (r: range) : numRange option = match (nr, r) with
  (nrStart, nrLength), (d, s, len) ->
    let overlap a1 a2 b1 b2 =
      let low = Int.max a1 b1 in
      let high = Int.min a2 b2 in
      if low <= high then Some (low, (high - low)) else None
    in match overlap nrStart (nrStart + nrLength) s (s + len) with
    | Some (start, length) -> Some (d + (start - s), length)
    | None -> None

let applyMapToNumRange (nr: numRange) (m: map) : numRange list = match List.filter_map (applyRangeToNumRange nr) m with
  | [] -> [nr]
  | l -> l
let applyMapToNumRanges (nrl: numRange list) (m: map) : numRange list = List.flatten (List.map (fun nr -> applyMapToNumRange nr m) nrl)

let rec parseSeedRanges (seeds: int list) : numRange list = match seeds with
  | a :: b :: rest -> (a, b) :: (parseSeedRanges rest)
  | [] -> []
  | _ -> invalid_arg "bad seed list"

let seedRanges = parseSeedRanges seeds
let locationRanges = List.fold_left applyMapToNumRanges seedRanges maps
let () = Printf.printf "\n\npart2 lowest location #: %d" (List.fold_left Int.min Int.max_int (List.map fst locationRanges))