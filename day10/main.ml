let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

type node = int * int
type neighbors = (node * node) option
module NodeMap = Map.Make(struct type t = node let compare = compare end) (* map where keys are nodes *)
type graphType = neighbors NodeMap.t (* dict where keys are nodes and vals are neighbors *)

let indexedLines = lines |>
  List.mapi (fun index str -> List.rev
    (String.fold_left (fun acc c ->
      (c, (List.length acc), index) :: acc)
    [] str)) (* indexedLines is type (char * int * int) list list, where each value is the char + its x and y indices *)
    (* in this coordinate system (0, 0) is at the top left, increasing x goes right, increasing y goes down *)

let buildGraph (g: graphType) (n: char * int * int): graphType =
  let c, x, y = n in
  let nodeNeighbors: neighbors = match c with
  | '.' -> None                          (* .: no neighbors *)
  | '|' -> Some ((x, y - 1), (x, y + 1)) (* pipe: neighbors are above and below *)
  | '-' -> Some ((x - 1, y), (x + 1, y)) (* dash: neighbors are left and right *)
  | 'L' -> Some ((x, y - 1), (x + 1, y)) (* L: neighbors are above and right *)
  | 'J' -> Some ((x, y - 1), (x - 1, y)) (* J: neighbors are above and left *)
  | '7' -> Some ((x, y + 1), (x - 1, y)) (* 7: neighbors are below and left *)
  | 'F' -> Some ((x, y + 1), (x + 1, y)) (* F: neighbors are below and right *)
  | 'S' -> None in                       (* S: there are neighbors but we don't know what they are yet *)
  NodeMap.add (x, y) nodeNeighbors g

let graph: graphType = List.fold_left (List.fold_left buildGraph) (NodeMap.empty) indexedLines

let _, startX, startY = List.flatten indexedLines |> List.find (fun (c, _, _) -> c = 'S')
let startNeighbor1 :: startNeighbor2 :: [] = graph
  |> NodeMap.bindings
  |> List.filter (fun (_, neighbors) -> match neighbors with | None -> false | Some (n1, n2) -> (startX, startY) = n1 || (startX, startY) = n2)
  |> List.map fst
let graph = NodeMap.add (startX, startY) (Some (startNeighbor1, startNeighbor2)) graph

let rec buildCycle (l: node list) (last: node) (now: node) (graph: graphType) : node list =
  if List.mem now l then List.rev l else
    let next = match (NodeMap.find now graph) with
    | Some (n1, n2) -> if last = n1 then n2 else n1 in (* the next node is the neighbor of 'now' that isn't 'last' *)
    buildCycle (now :: l) now next graph

let startNode :: cycle = buildCycle [(startX, startY)] (startX, startY) startNeighbor1 graph
let farthestNodeDistance = cycle
  |> List.combine (List.rev cycle)
  |> List.mapi (fun index e -> (index, e))
  |> List.find (fun (_, (x, y)) -> x = y)
  |> fst
  |> (+) 1

let () = Printf.printf "farthest node distance: %d" farthestNodeDistance
(* solution to part 1 above *)