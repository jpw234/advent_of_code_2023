let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []
let indexedLines: (char * int * int) list list =
  List.mapi (fun index str -> List.rev (String.fold_left (fun acc c -> (c, (List.length acc), index) :: acc) [] str)) lines

type galaxy = int * int
module GalaxySet = Set.Make(struct type t = galaxy let compare = compare end)

let buildGalaxySet (s: GalaxySet.t) ((c, x, y): char * int * int): GalaxySet.t = if c = '#' then GalaxySet.add (x, y) s else s
let galaxies: GalaxySet.t = List.fold_left (List.fold_left buildGalaxySet) GalaxySet.empty indexedLines

let originalRowCount = List.length lines
let originalColCount = String.length (List.hd lines)

let rec expandColumns (c: int) (ef: int) (g: GalaxySet.t) = (* ef = expansion factor, introduced to solve part 2 *)
  if c = -1 then g else
    if GalaxySet.exists (fun (_, y) -> c = y) g then expandColumns (c - 1) ef g (* column is not empty *)
    else expandColumns (c - 1) ef (GalaxySet.map (fun (x, y) -> if y > c then (x, y + ef) else (x, y)) g)
let rec expandRows (r: int) (ef: int) (g: GalaxySet.t) =
  if r = -1 then g else
    if GalaxySet.exists (fun (x, _) -> r = x) g then expandRows (r - 1) ef g (* row is not empty *)
    else expandRows (r - 1) ef (GalaxySet.map (fun (x, y) -> if x > r then (x + ef, y) else (x, y)) g)

let expandedGalaxies: GalaxySet.t = galaxies |> expandColumns (originalColCount - 1) 1 |> expandRows (originalRowCount - 1) 1

let rec pairs (l: 'a list) : ('a * 'a) list = match l with
  | [] -> []
  | hd :: tl -> (List.map (fun e -> (hd, e)) tl) @ (pairs tl)

let shortestDistance (pair: galaxy * galaxy) : int = match pair with
  ((g1x, g1y), (g2x, g2y)) -> (Int.abs (g1x - g2x)) + (Int.abs (g1y - g2y))

let () = Printf.printf "path sum = %d" (expandedGalaxies |> GalaxySet.elements |> pairs |> List.map shortestDistance |> List.fold_left (+) 0)
(* part 1 solution above *)

let expandedGalaxies2: GalaxySet.t = galaxies |> expandColumns (originalColCount - 1) 999999 |> expandRows (originalRowCount - 1) 999999

let () = Printf.printf "\n\npart 2 path sum = %d" (expandedGalaxies2 |> GalaxySet.elements |> pairs |> List.map shortestDistance |> List.fold_left (+) 0)