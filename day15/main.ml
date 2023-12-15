let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

let steps = String.split_on_char ',' (List.nth lines 0)

let hashAlg (acc: int) (c: char) = ((acc + (Char.code c)) * 17) mod 256

let () = Printf.printf "part 1 sum: %d" (steps |> List.map (String.fold_left hashAlg 0) |> List.fold_left (+) 0)

type lens = string * int
type op = EQUALS | DASH
type step = string * op * int option

let parseStep (s: string) : step = if String.contains s '='
  then let label :: len :: [] = String.split_on_char '=' s in (label, EQUALS, Some (int_of_string len))
  else let label :: _ :: [] = String.split_on_char '-' s in (label, DASH, None)

module IntMap = Map.Make(struct type t = int let compare = compare end) (* map where keys are ints *)
type boxes = (lens list) IntMap.t (* map where keys are ints and values are lists of lenses (represents the 256 boxes) *)
let initState: boxes = List.fold_left (fun m i -> IntMap.add i [] m) IntMap.empty (List.init 256 (fun x -> x))

let performStep (b: boxes) (s: step) : boxes = match s with
  | label, DASH, None -> let boxNum = String.fold_left hashAlg 0 label in
      b
      |> IntMap.find boxNum
      |> List.filter (fun (l, _) -> not (String.equal l label))
      |> (fun newL -> IntMap.add boxNum newL b)
  | label, EQUALS, Some fLen -> let boxNum = String.fold_left hashAlg 0 label in
      b
      |> IntMap.find boxNum
      |> (fun list ->
            if List.exists (fun (l, _) -> l = label) list
              then List.map (fun (l, fl) -> if l = label then (l, fLen) else (l, fl)) list
              else list @ [(label, fLen)])
      |> (fun newL -> IntMap.add boxNum newL b)

let deriveFocusingPower (b: boxes) : int = b
  |> IntMap.bindings (* (int * lens list) list *)
  |> List.fold_left (
      fun acc (boxNum, lensL) -> acc + fst
        (List.fold_left (fun (acc, index) (_, fl) -> acc + ((boxNum + 1) * index * fl), index + 1) (0, 1) lensL)) 0

let () = Printf.printf "\n\npart 2: total focusing power = %d"
  (steps |> List.map parseStep |> List.fold_left performStep initState |> deriveFocusingPower)