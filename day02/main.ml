(* Start by defining the problem space in terms of types *)
type color = Blue | Red | Green
type colorCount = int * color
type pullResult = colorCount list
type game = int * pullResult list
type gameConstraint = colorCount * colorCount * colorCount

let gameIsValid (limit: gameConstraint) (g: game) : bool = (* check if a game is valid given a gameConstraint *)
  match limit with
  | (x, Red), (y, Green), (z, Blue) ->
    match g with (_, pr) ->
      let colorCountIsValid (inp: colorCount) : bool =
        match inp with
        | (c, Red) -> (c <= x)
        | (c, Green) -> (c <= y)
        | (c, Blue) -> (c <= z)
      in List.for_all (List.for_all colorCountIsValid) pr
  | _ -> invalid_arg "gameConstraint must include exactly one limit on each color"

(* now write the functions we need to parse a string into a game *)
let extractGameId (g: string) : int = int_of_string (List.nth (String.split_on_char ' ' g) 1) (* input like "Game 1", pulls what's after the space *)
let extractColorCount (g: string) : colorCount = (* input like "10 green" *)
  match String.split_on_char ' ' g with
  | x :: "green" :: []-> int_of_string x, Green
  | x :: "red" :: [] -> int_of_string x, Red
  | x :: "blue" :: [] -> int_of_string x, Blue
  | _ -> invalid_arg ("invalid colorCount string " ^ g)
let extractPullResult (g: string) : pullResult = (* input like "10 green, 4 red"*)
  String.split_on_char ',' g |> List.map String.trim |> List.map extractColorCount
let parseGame (str: string) : game =
  match String.split_on_char ':' str with
  | gameId :: pulls :: [] ->
    extractGameId gameId, (String.split_on_char ';' pulls |> List.map String.trim |> List.map extractPullResult)
  | _ -> invalid_arg "invalid game string"

let data = open_in "input.txt"
let rec read_lines i acc = match (try Some (input_line i) with End_of_file -> None) with Some str -> read_lines i (str :: acc) | None -> acc
let lines = read_lines data []
let () = close_in data

let games = List.map parseGame lines
let validGames = List.filter (gameIsValid ((12, Red), (13, Green), (14, Blue))) games
let validGameIdSum =
  let gameSummer acc game = match game with (id, _) -> id + acc in
  List.fold_left gameSummer 0 validGames

let () = Printf.printf "Valid game id sum: %d" validGameIdSum
(* part 1 solution above *)

let minConstraint (gc: gameConstraint) (i: colorCount) : gameConstraint = match gc with
  | (x, Red), (y, Green), (z, Blue) ->
    match i with
    | (c, Red) -> if c > x then ((c, Red), (y, Green), (z, Blue)) else gc
    | (c, Green) -> if c > y then ((x, Red), (c, Green), (z, Blue)) else gc
    | (c, Blue) -> if c > z then ((x, Red), (y, Green), (c, Blue)) else gc
  | _ -> invalid_arg "invalid game constraint"

let calculateGamePower (g: game) : int = match g with
  | (_, pr) ->
    match (List.fold_left (List.fold_left minConstraint) ((0, Red), (0, Green), (0, Blue)) pr) with
    | (x, Red), (y, Green), (z, Blue) -> x * y * z

let () = Printf.printf "\n\nGame power sum: %d" (games |> List.map calculateGamePower |> List.fold_left (+) 0)