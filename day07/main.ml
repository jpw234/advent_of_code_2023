let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; acc
let lines: string list = List.rev (read_lines (open_in "input.txt") [])

type card = int
type bid = int
type hand = card list
type handType = FiveKind | FourKind | FullHouse | ThreeKind | TwoPair | Pair | High
let handTypeValue ht = match ht with FiveKind -> 7 | FourKind -> 6 | FullHouse -> 5 | ThreeKind -> 4 | TwoPair -> 3 | Pair -> 2 | High -> 1
type line = hand * bid

let parseHand (s: string) : hand = List.rev (String.fold_left (fun acc c -> let cardVal = (match c with
  | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'T' -> 10 | 'J' -> 11 | 'Q' -> 12 | 'K' -> 13 | 'A' -> 14 | _ -> invalid_arg "bad card")
  in cardVal :: acc) [] s)
let parseLine (s: string) : line = match String.split_on_char ' ' s with
  | hand :: bid :: [] -> parseHand hand, int_of_string bid | _ -> invalid_arg "bad line"

let allHands = List.map parseLine lines

let getHandType (h: hand) : handType =
  let rankArray = Array.make 13 0 in
  List.iter (fun c -> Array.set rankArray (c - 2) ((Array.get rankArray (c - 2)) + 1)) h;
  match (Array.fold_left Int.max 0 rankArray) with
  | 5 -> FiveKind
  | 4 -> FourKind
  | 3 -> if (Array.mem 2 rankArray) then FullHouse else ThreeKind
  | 2 -> if (Array.fold_left (fun acc e -> if (e = 2) then acc + 1 else acc) 0 rankArray) = 2 then TwoPair else Pair
  | 1 -> High
  | _ -> invalid_arg "bad formatted hand"

let rec intListCompare (l1: int list) (l2: int list) : int = match (l1, l2) with
  | h1 :: t1, h2 :: t2 -> if (compare h1 h2) != 0 then compare h1 h2 else intListCompare t1 t2
  | [], [] -> 0
  | _ -> invalid_arg "lists to compare must be the same length"

let handCompare (h1: hand) (h2: hand) : int =
  let rel = compare (handTypeValue (getHandType h1)) (handTypeValue (getHandType h2)) in
  if rel = 0
    then intListCompare h1 h2
    else rel

let sortedHands = List.sort (fun h1 h2 -> handCompare (fst h1) (fst h2)) allHands
let totalWinnings = sortedHands |> List.mapi (fun index h -> (index + 1) * (snd h)) |> List.fold_left (+) 0

let () = Printf.printf "total winnings: %d" totalWinnings
(* solution to part 1 above *)

let parseHand2 (s: string) : hand = List.rev (String.fold_left (fun acc c -> let cardVal = (match c with
  | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'T' -> 10 | 'J' -> 1 | 'Q' -> 11 | 'K' -> 12 | 'A' -> 13 | _ -> invalid_arg "bad card")
  in cardVal :: acc) [] s)
let parseLine2 (s: string) : line = match String.split_on_char ' ' s with
  | hand :: bid :: [] -> parseHand2 hand, int_of_string bid | _ -> invalid_arg "bad line"

let allHands2 = List.map parseLine2 lines

let getHandType2 (h: hand) : handType =
  let rankArray = Array.make 13 0 in
  List.iter (fun c -> Array.set rankArray (c - 1) ((Array.get rankArray (c - 1)) + 1)) h;
  let jokers :: others = Array.to_list rankArray in
  match (jokers, (List.fold_left Int.max 0 others)) with
  | 0, 5 | 1, 4 | 2, 3 | 3, 2 | 4, 1 | 5, 0 -> FiveKind
  | 0, 4 | 1, 3 | 2, 2 | 3, 1 -> FourKind
  | 0, 3 -> if (List.mem 2 others) then FullHouse else ThreeKind
  | 1, 2 -> if (List.fold_left (fun acc e -> if (e = 2) then acc + 1 else acc) 0 others) = 2 then FullHouse else ThreeKind
  | 2, 1 -> ThreeKind
  | 0, 2 -> if (List.fold_left (fun acc e -> if (e = 2) then acc + 1 else acc) 0 others) = 2 then TwoPair else Pair
  | 1, 1 -> Pair
  | 0, 1 -> High
  | _ -> invalid_arg ("bad hand")

  let handCompare2 (h1: hand) (h2: hand) : int =
    let rel = compare (handTypeValue (getHandType2 h1)) (handTypeValue (getHandType2 h2)) in
    if rel = 0
      then intListCompare h1 h2
      else rel
  
  let sortedHands2 = List.sort (fun h1 h2 -> handCompare2 (fst h1) (fst h2)) allHands2
  let totalWinnings2 = sortedHands2 |> List.mapi (fun index h -> (index + 1) * (snd h)) |> List.fold_left (+) 0
  
  let () = Printf.printf "\n\ntotal winnings part 2: %d" totalWinnings2
  