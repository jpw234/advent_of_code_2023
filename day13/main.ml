let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

module CoordMap = Map.Make(struct type t = int * int let compare = compare end) (* map where keys are x,y coordinates *)
type grid = char CoordMap.t (* dict where keys are coordinates and vals are chars *)

let grids: grid list =
  let gl, g, _ = List.fold_left (fun (gl, g, rnum) s -> match s with
    | "" -> (g :: gl, CoordMap.empty, 0)
    | line ->
        let (updatedGrid, _) = String.fold_left (fun (g, i) c -> (CoordMap.add (i, rnum) c g, i + 1)) (g, 0) line in
        (gl, updatedGrid, rnum + 1)) ([], CoordMap.empty, 0) lines
  in List.rev (g :: gl)

type reflection = Horizontal of int | Vertical of int

let gridToRows (g: grid) : char list list =
  let gridVals = CoordMap.bindings g in
  let rec extractRow (c: int) =
    let matchingVals = List.filter (fun ((_, y), _) -> y = c) gridVals in
    if List.length matchingVals = 0
      then []
      else (List.map snd (List.sort (fun ((x1, _), _) ((x2, _), _) -> compare x1 x2) matchingVals)) :: extractRow (c + 1)
  in extractRow 0

let gridToColumns (g: grid) : char list list =
  let gridVals = CoordMap.bindings g in
  let rec extractColumn (c: int) =
    let matchingVals = List.filter (fun ((x, _), _) -> x = c) gridVals in
    if List.length matchingVals = 0
      then []
      else (List.map snd (List.sort (fun ((_, y1), _) ((_, y2), _) -> compare y1 y2) matchingVals)) :: extractColumn (c + 1)
  in extractColumn 0

let slice (first: int) (last: int) (l: 'a list) : 'a list = List.filteri (fun i _ -> i >= first && i <= last) l

let findReflection (noteq: reflection option) (g: grid) : reflection option =
  let rec reflectionFromGrid (v: int) (ls: char list list) (isHorizontal: bool) : reflection option =
    let len = List.length ls in
    if v >= len - 1 then None (* terminate *)
    else
      let overHalf = v >= (len / 2) in
      let sliceSize = if overHalf then (len - 1 - v) else v + 1 in
      let l1 = if overHalf then slice (v + 1) (len - 1) ls else slice 0 v ls in
      let l2 = if overHalf then slice (v - sliceSize + 1) v ls else slice (v + 1) (v + sliceSize) ls in
      if compare l1 (List.rev l2) = 0
        then
          let candVal = if isHorizontal then Some (Horizontal v) else Some (Vertical v) in
          if compare candVal noteq != 0 then candVal else reflectionFromGrid (v + 1) ls isHorizontal
        else reflectionFromGrid (v + 1) ls isHorizontal
  in
  let hz = (reflectionFromGrid 0 (gridToRows g) true) in
  if Option.is_some hz then hz else (reflectionFromGrid 0 (gridToColumns g) false)

let reflectionFolder acc r = match r with
  | Some (Horizontal x) -> acc + (100 * (x + 1))
  | Some (Vertical y) -> acc + (y + 1)
  | None -> acc

let () = Printf.printf "part 1 sum = %d" (grids |> List.map (findReflection None) |> List.fold_left reflectionFolder 0)
(* part 1 solution above *)

let findSmudgedReflection (g: grid) : reflection option =
  let oldReflection = findReflection None g in
  let gridVals = CoordMap.bindings g in
  List.find_map (fun ((x, y), c) ->
    let newGrid = CoordMap.add (x, y) (if c = '.' then '#' else '.') g in
    findReflection oldReflection newGrid
    ) gridVals

let () = Printf.printf "\n\npart 2 sum = %d" (grids |> List.map findSmudgedReflection |> List.fold_left reflectionFolder 0)