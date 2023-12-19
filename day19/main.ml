let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

type part = { x: int; m: int; a: int; s: int }
type outcome = Accept | Reject
type condition = char * char * int
type rule = (condition option) * ((outcome, string) Either.t)
type workflow = rule list

module StringMap = Map.Make(struct type t = string let compare = compare end) (* map where keys are labels (strings) *)
type workflows = workflow StringMap.t (* dict where keys are strings and vals are workflows *)

let partVal p c = match c with | 'x' -> p.x | 'm' -> p.m | 'a' -> p.a | 's' -> p.s | _ -> invalid_arg "bad part prop"

let rec processPart (wm: workflows) (l: string) (p: part) : outcome =
  let processWorkflow (label: string) =
    let processRule (r: rule) = match r with
      | None, o -> Some o
      | Some (prop, '<', v), o -> if (partVal p prop) < v then Some o else None
      | Some (prop, '>', v), o -> if (partVal p prop) > v then Some o else None
      | _ -> invalid_arg "bad rule" in
    let rec processRuleList (rl: rule list) = let hd :: tl = rl in match processRule hd with
      | None -> processRuleList tl
      | Some o -> o in
    processRuleList (StringMap.find label wm) in
  match processWorkflow l with
  | Left outcome -> outcome
  | Right newLabel -> processPart wm newLabel p

let (partList, workflowList) = lines |> List.filter (fun s -> (String.length s) > 0) |> List.partition (fun s -> (String.get s 0) = '{')
let parsePart (s: string) : part = Scanf.sscanf s "{x=%i,m=%i,a=%i,s=%i}" (fun x m a s -> { x; m; a; s })
let parseWorkflow (s: string) : string * workflow =
  Scanf.sscanf s "%s@{%s@}" (fun lbl rest ->
    let rl = String.split_on_char ',' rest in
    let parseResult (res: string) : ((outcome, string) Either.t) = match res with "A" -> Left Accept | "R" -> Left Reject | l -> Right l in
    let parseCondition (c: string) : condition =
      let partVal = String.get c 0 in
      let op = String.get c 1 in
      let num = int_of_string (String.sub c 2 ((String.length c) - 2)) in
      partVal, op, num in
    let parseRule (r: string) : rule = match String.split_on_char ':' r with
      | res :: [] -> None, parseResult res
      | cond :: o :: [] -> Some (parseCondition cond), parseResult o in
    lbl, List.map parseRule rl)

let workflowMap : workflows = workflowList
  |> List.map parseWorkflow
  |> List.fold_left (fun acc (lbl, wf) -> StringMap.add lbl wf acc) StringMap.empty

let () = Printf.printf "accepted part rating number sum: %d" (
  partList
  |> List.map parsePart
  |> List.filter (fun p -> (processPart workflowMap "in" p) = Accept)
  |> List.map (fun p -> p.x + p.m + p.a + p.s)
  |> List.fold_left (+) 0
)
(* part 1 solution above *)