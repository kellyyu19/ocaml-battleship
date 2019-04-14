open Battleship
open State
open Command




let cmdToTupleFire command =
  match command with
  | Fire list when (List.length list = 1) -> let coord = List.nth list 0 in 
    (String.get coord 0,int_of_char (String.get coord 1))
  | _ -> raise Malformed

let string_to_ship str = 
  match str with 
  | "battleship" -> init_battleship
  | "cruiser" -> init_cruiser
  | "carrier" -> init_carrier
  | "submarine" -> init_submarine
  | "destroyer" -> init_destroyer
  | _ -> raise Malformed

let cmdToShip command = 
  match command with 
  | Place list when (List.length list = 3) -> 
    (match List.nth list 0 with 
     | "carrier" -> string_to_ship "carrier"
     | "battleship" -> string_to_ship "battleship"
     | "cruiser" -> string_to_ship "cruiser"
     | "submarine" -> string_to_ship "submarine"
     | "destroyer" -> string_to_ship "destroyer"
     | _ -> raise Malformed)
  | _ -> raise Malformed

let cmdToCoordOne command = 
  match command with 
  | Place list when (List.length list = 3) -> let coord = List.nth list 1 in 
    (String.get coord 0,int_of_char (String.get coord 1))
  | _ -> raise Malformed

let cmdToCoordTwo command = 
  match command with 
  | Place list when (List.length list = 3) -> let coord = List.nth list 2 in 
    (String.get coord 0,int_of_char (String.get coord 1))
  | _ -> raise Malformed



let rec play_game_helper state_p1 state_p2 turn =  
  if (placing state_p1) then 
    ANSITerminal.print_string [ANSITerminal.Foreground Blue] 
      "\n Player 1, please place your next ship. Ships remaining: " ^ queue state_p1;
  let command = parse (read_line ()) in 
  let ship = cmdToShip command in 
  let coordOne = cmdToCoordOne command in 
  let coordTwo = cmdToCoordTwo command in 
  play_game_helper (place ship coordOne coordTwo state_p1) state_p2 turn
else if (placing state_p2) then
  ANSITerminal.print_string [ANSITerminal.Foreground Blue] 
    "\n Player 2, please place your next ship. Ships remaining: " ^ queue state_p1;
let command = parse (read_line ()) in 
let ship = cmdToShip command in 
let coordOne = cmdToCoordOne command in 
let coordTwo = cmdToCoordTwo command in 
play_game_helper state_p1 (place ship coordOne coordTwo state_p2) turn
else 
  try 
    let userInput  = parse read_line () in
    match userInput with 
    | Fire coord -> fire cmdToTupleFire (if turn then state_p1 else state_p2)
    | Status ->
    | Quit -> 
  with 
  | Malformed -> print_endline "\n That was not a valid command.\n";
    play_game_helper state_p1 state_p2 *) 



(* let play_game start = 
   play_game_helper init_state init_state *)


(* let main () = 
   ANSITerminal.print_string [ANSITerminal.Foreground Blue] "\n Battleship\n";
   print_endline "\n Please type start to play a new game.\n ";
   print_string ">>>";
   let start = read_line () in play_game start *)


(* let () = main ()