let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; acc
let lines: string list = List.rev (read_lines (open_in "input.txt") [])

module IntSet = Set.Make(Int);;
type card = int * IntSet.t * IntSet.t (* card ID, winning numbers, numbers you have *)

let numWinners (c: card) = match c with (_, w, n) -> IntSet.cardinal (IntSet.inter w n)
let rec cardValue (numWinners: int) = match numWinners with
  | 0 -> 0
  | 1 -> 1
  | _ -> 2 * (cardValue (numWinners - 1))

let removeEmptyStrings (l: string list) : string list = List.filter (fun s -> String.length s > 0) l
let parseCardId s = int_of_string (List.nth (removeEmptyStrings (String.split_on_char ' ' s)) 1)
let parseCard (s: string) : card = match String.split_on_char ':' s with
  | cardId :: sets :: [] ->
    (match (String.split_on_char '|' sets
      |> List.map (String.split_on_char ' ')
      |> List.map removeEmptyStrings
      |> List.map (List.map int_of_string)) with
    | winners :: numbers :: [] -> parseCardId cardId, IntSet.of_list winners, IntSet.of_list numbers
    | _ -> invalid_arg "bad card string: failed to parse sets")
  | _ -> invalid_arg "bad card string: failed to parse cardId"

let totalCardValue = lines
  |> List.map parseCard
  |> List.map numWinners
  |> List.map cardValue
  |> List.fold_left (+) 0

let () = Printf.printf "total card value: %d" totalCardValue
(* part 1 solution above *)

module IntMap = Map.Make(struct type t = int let compare = compare end) (* dict where keys are ints *)
type intToIntMap = int IntMap.t (* dict where keys and values are ints *)
(* for part 2, implement a dynamic programming style algorithm *)

let rec part2Winners (cardId: int) (numWinners: int) (m: intToIntMap) : int = match numWinners with
  | 0 -> 1
  | _ -> (IntMap.find (cardId + numWinners) m) + (part2Winners cardId (numWinners - 1) m)

let part2Folder (c: card) (acc: int * (intToIntMap)) = match c, acc with
  | (id, _, _), (currTotal, currMap) -> let cardTotal = part2Winners id (numWinners c) currMap
  in (currTotal + cardTotal, IntMap.add id cardTotal currMap)

let part2CardValues = fst (List.fold_right part2Folder (List.map parseCard lines) (0, IntMap.empty))
let () = Printf.printf "\ntotal scratchcards: %d" part2CardValues