let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> close_in i; List.rev acc
let lines: string list = read_lines (open_in "input.txt") []

type direction = Up | Down | Right | Left
type step = direction * int * string

let parseLine (s: string) : step = Scanf.sscanf s "%c %d (#%s@)" (fun a b c -> let dir = (match a with
  | 'U' -> Up | 'D' -> Down | 'R' -> Right | 'L' -> Left) in dir, b, c)

let generatePolygonCoordinates (sl: step list) : (int * int) list =
  let nextCoord (l: (int * int) list) ((dir, len, _): step) =
    let (lastX, lastY) :: _ = l in
    match dir with
    | Up -> (lastX, lastY + len) :: l
    | Down -> (lastX, lastY - len) :: l
    | Right -> (lastX + len, lastY) :: l
    | Left -> (lastX - len, lastY) :: l
  in List.rev (List.fold_left nextCoord [(0, 0)] sl)

let areaOfPolygon (cl: (int * int) list) : int =
  let rec aux l = match l with
    | (x1, y1) :: (x2, y2) :: rest -> (x1 * y2) - (y1 * x2) + aux ((x2, y2) :: rest)
    | _ :: [] -> 0
    | [] -> invalid_arg "bad input to areaOfPolygon" in
  Int.abs ((aux cl) / 2)
(* i guess you have to adjust by adding the perimeter / 2 + 1, since this calc goes 'through' the perimeter squares *)

let perimeterOfPolygon (cl: (int * int) list) : int =
  let rec aux l = match l with
    | (x1, y1) :: (x2, y2) :: rest -> (Int.abs ((x1 - x2) + (y1 - y2))) + aux ((x2, y2) :: rest) (* no sqrts needed b/c rectangular *)
    | _ :: [] -> 0
    | [] -> invalid_arg "bad input to perimeterOfPolygon" in
  aux cl

let actualArea cl = (areaOfPolygon cl) + ((perimeterOfPolygon cl) / 2) + 1
let () = Printf.printf "part 1 solution: %d" (lines |> List.map parseLine |> generatePolygonCoordinates |> actualArea)

let parseLine2 s = Scanf.sscanf s "%c %d (#%s@)" (fun _ _ s ->
  let dir = (match (s.[5]) with
    | '0' -> Right | '1' -> Down | '2' -> Left | '3' -> Up | _ -> invalid_arg ("\nbad input to parseLine2, string " ^ s)) in
  let len = int_of_string ("0x" ^ (String.sub s 0 5)) in
  dir, len, "")

let () = Printf.printf "\n\npart 2 solution: %d" (lines |> List.map parseLine2 |> generatePolygonCoordinates |> actualArea)