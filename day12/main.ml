let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

type springCondition = Operational | Damaged
type maybeCondition = Known of springCondition | Unknown
type groupConstraint = int list

let parseSpringList (s: string) : maybeCondition list = String.fold_right (fun c acc -> match c with
  | '#' -> Known Damaged :: acc
  | '.' -> Known Operational :: acc
  | '?' -> Unknown :: acc
  | _ -> invalid_arg "bad spring record") s [Known Operational] (* appending '.' to the end makes the solver easier *)
let parseGroupConstraint (s: string) : groupConstraint = s |> String.split_on_char ',' |> List.map int_of_string
let parseLine (s: string) = let springs :: groups :: [] = String.split_on_char ' ' s in parseSpringList springs, parseGroupConstraint groups

let memo_rec f = (* helper function for memoizing a recursive fn, taken from https://github.com/Fubuchi/advent-of-code/blob/99c8c0d9d9f23dd6311e1031e855fd3f1461459f/ocaml/lib/share.ml#L61-L70 *)
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g

let solver self = function
| ([], [], 0) -> 1 (* one solution for base case - empty spring list + empty constraint *)
| ([], _, 0) -> 0 (* if there's a remaining constraint but no remaining springs, no solution *)
| ([], [], _) -> 0 (* if there's a remaining constraint but no remaining springs, no solution *)
| (s :: l, gc, filled) -> (match s with
  | Known Damaged -> self (l, gc, filled + 1) (* if we see a damaged spring add it to the current 'filled' count *)
  | Known Operational -> (*if we see an operational spring we need to potentially pop the 'filled' count *)
    if filled > 0 then
      match gc with
      | c :: rest when c = filled -> self (l, rest, 0) (* if the filled count matches the top constraint, pop and continue *)
      | _ -> 0 (* if the filled count doesn't match the top constraint, no solution *)
    else self (l, gc, 0)
  | Unknown -> self ((Known Damaged) :: l, gc, filled) + self ((Known Operational) :: l, gc, filled)) (* sum both possibilities *)
let memoized_solver = memo_rec solver

let () = Printf.printf "sum counts = %d" (lines
  |> List.map parseLine
  |> List.map (fun l -> memoized_solver (fst l, snd l, 0))
  |> List.fold_left (+) 0)
(* part 1 solution above *)

let expander (s: string) : string = let springs :: groups :: [] = String.split_on_char ' ' s in
  let expandedSprings = springs ^ "?" ^ springs ^ "?" ^ springs ^ "?" ^ springs ^ "?" ^ springs in
  let expandedGroups = groups ^ "," ^ groups ^ "," ^ groups ^ "," ^ groups ^ "," ^ groups in
  expandedSprings ^ " " ^ expandedGroups

let () = Printf.printf "\n\npart 2 sum counts = %d" (lines
  |> List.map expander
  |> List.map parseLine
  |> List.map (fun l -> memoized_solver (fst l, snd l, 0))
  |> List.fold_left (+) 0)