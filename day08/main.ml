let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

type node = string
type nodePair = string * string
module StringMap = Map.Make(struct type t = node let compare = compare end) (* dict where keys are nodes (strings) *)
type nodeSet = nodePair StringMap.t (* dict where keys are strings and values are nodePairs *)

(* hideous lol but idk how to do regex in OCaml *)
let parseLine (s: string) : (node * nodePair) = Scanf.sscanf s "%[A-Z]%[A-Z]%[A-Z] = (%[A-Z]%[A-Z]%[A-Z], %[A-Z]%[A-Z]%[A-Z])" (fun a b c d e f g h i -> (a ^ b ^ c), ((d ^ e ^ f), (g ^ h ^ i)))

let instructionSet :: _ :: nodeLines = lines
let graph: nodeSet = nodeLines
  |> List.map parseLine
  |> List.fold_left (fun map l -> StringMap.add (fst l) (snd l) map) StringMap.empty

let runInstructionSet (is: string) (graph: nodeSet) (start: node) : node =
  String.fold_left (fun pos i -> match i, (StringMap.find pos graph) with
    | 'R', (_, next) -> next
    | 'L', (next, _) -> next) start is

let rec stepsToFinish (is: string) (graph: nodeSet) (timesRun: int) (start: node) : int = match runInstructionSet is graph start with
  | "ZZZ" -> (timesRun + 1) * (String.length is)
  | other -> stepsToFinish is graph (timesRun + 1) other

let () = Printf.printf "steps to finish: %d" (stepsToFinish instructionSet graph 0 "AAA")
(* part 1 solution above *)

let startingNodes = graph |> StringMap.bindings |> List.map fst |> List.filter (fun s -> (String.get s 2) = 'A')

let rec stepsToFinish2 (is: string) (graph: nodeSet) (timesRun: int) (start: node) : int = 
  let finNode = runInstructionSet is graph start in
  if (String.get finNode 2) = 'Z'
    then (timesRun + 1) * (String.length is)
    else stepsToFinish2 is graph (timesRun + 1) finNode

let lcm (nums: int list) : int =
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  let lcm a b = (a * b) / (gcd a b) in
  List.fold_left lcm 1 nums

let () = Printf.printf "\n\nsteps to finish (part 2): %d" (startingNodes |> List.map (stepsToFinish2 instructionSet graph 0) |> lcm)