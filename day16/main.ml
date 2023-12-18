let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

type coordinate = int * int (* x, y *)
module CoordMap = Map.Make(struct type t = coordinate let compare = compare end) (* map where keys are x,y coordinates *)
type grid = (char * bool) CoordMap.t (* dict where keys are coordinates and vals are (char * bool) *)

let inputGrid, _ = List.fold_left
  (fun (g, rnum) s ->
    let updatedGrid, _ = String.fold_left (fun (g, i) c -> (CoordMap.add (i, rnum) (c, false) g, i + 1)) (g, 0) s in
    updatedGrid, rnum + 1)
  (CoordMap.empty, 0)
  lines

type direction = Left | Right | Up | Down
type step = direction * coordinate

let processStep (g: grid) ((d, (x, y)): step) : step list = match CoordMap.find_opt (x, y) g with None -> [] | Some (m, _) -> (match (d, m) with
  | Left, '.' -> [(Left, (x - 1, y))] | Right, '.' -> [(Right, (x + 1, y))] | Up, '.' -> [(Up, (x, y - 1))] | Down, '.' -> [(Down, (x, y + 1))]
  | Left, '-' -> [(Left, (x - 1, y))] | Right, '-' -> [(Right, (x + 1, y))] | Up, '|' -> [(Up, (x, y - 1))] | Down, '|' -> [(Down, (x, y + 1))]
  (* all the above let the beam continue as normal. Now do the bounces ('/' and '\') *)
  | Left, '/' -> [(Down, (x, y + 1))]   | Left, '\\' -> [(Up, (x, y - 1))]
  | Right, '/' -> [(Up, (x, y - 1))]    | Right, '\\' -> [(Down, (x, y + 1))]
  | Up, '/' -> [(Right, (x + 1, y))]    | Up, '\\' -> [(Left, (x - 1, y))]
  | Down, '/' -> [(Left, (x - 1, y))]   | Down, '\\' -> [(Right, (x + 1, y))]
  (* now the splits *)
  | Left, '|' | Right, '|' -> [(Up, (x, y - 1)); (Down, (x, y + 1))]
  | Up, '-' | Down, '-' -> [(Left, (x - 1, y)); (Right, (x + 1, y))]
)

let setVisited (sl: step list) (g: grid) : grid =
  let updater e = Option.map (fun (c, _) -> (c, true)) e in
  List.fold_left (fun acc (_, c) -> CoordMap.update c updater acc) g sl

let rec processSteps (sl: step list) (completedSteps: step list) (g: grid) = match sl with
  | [] -> g
  | _ ->
    let filteredSL = List.filter (fun s -> not (List.mem s completedSteps)) sl in
    let newG = setVisited filteredSL g in
    let newSL = List.flatten (List.map (processStep newG) filteredSL) in
    processSteps newSL (completedSteps @ filteredSL) newG

let () = Printf.printf "total energized tiles: %d" (inputGrid
  |> processSteps [(Right, (0, 0))] []
  |> CoordMap.bindings
  |> List.filter (fun (_, (_, b)) -> b = true)
  |> List.length)
(* part one solution above *)

let maxY = (List.length lines) - 1
let maxX = (String.length (List.nth lines 0)) - 1
let outerCoords = inputGrid |> CoordMap.bindings |> List.map fst |> List.filter (fun (x, y) -> x = 0 || x = maxX || y = 0 || y = maxY)
let rightStarts = outerCoords |> List.filter (fun (x, _) -> x = 0) |> List.map (fun c -> Right, c)
let downStarts = outerCoords |> List.filter (fun (_, y) -> y = 0) |> List.map (fun c -> Down, c)
let leftStarts = outerCoords |> List.filter (fun (x, _) -> x = maxX) |> List.map (fun c -> Left, c)
let upStarts = outerCoords |> List.filter (fun (_, y) -> y = maxY) |> List.map (fun c -> Up, c)
let allStarts = rightStarts @ downStarts @ leftStarts @ upStarts

let bestStart = allStarts
  |> List.map (fun s -> processSteps [s] [] inputGrid)
  |> List.map CoordMap.bindings
  |> List.map (List.filter (fun (_, (_, b)) -> b = true))
  |> List.map List.length
  |> List.fold_left Int.max 0

let () = Printf.printf "\n\ntotal energized tiles in best possible start: %d" bestStart