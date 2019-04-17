open Battleship
open State
open Command
open Textgrid

(** [cmdToTupleFire command] is a (char,int) tuple that takes a command of
    type Fire and converts it into a valid coordinate that is a (char,int) 
    Raises: [Malformed] if the command is an invalid Fire command. *)
let cmdToTupleFire command =
  match command with
  | Fire list when (List.length list = 1) -> let coord = List.nth list 0 in 
    if String.length coord = 2 then 
      (String.get coord 0, int_of_char (String.get coord 1) - 48)
    else if String.sub coord 1 2 = "10" then
      (String.get coord 0, 10)
    else raise Malformed
  | _ -> raise Malformed

(** [string_to_ship str] converts the string that represents a ship to 
    its corresponding ship. 
    Raises: [Malformed] if the [str] is not one of the ship's names. *)
let string_to_ship str = 
  match str with 
  | "battleship" -> init_battleship
  | "cruiser" -> init_cruiser
  | "carrier" -> init_carrier
  | "submarine" -> init_submarine
  | "destroyer" -> init_destroyer
  | _ -> raise Malformed

(** [cmdToShip command] converts [command] into the ship that corresponds 
    with the [command].
    Raises: [Malformed] if [command] is an invalid Place command *)
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

(** [cmdToCoordOne command] is a (char,int) tuple that takes a command of
    type Place and converts it into a valid coordinate that is a (char,int) 
    Raises: [Malformed] if the command is an invalid Place command. *)
let cmdToCoordOne command = 
  match command with 
  | Place list when (List.length list = 3) -> let coord = List.nth list 1 in 
    if String.length coord = 2 then 
      (String.get coord 0, int_of_char (String.get coord 1) - 48)
    else 
      (String.get coord 0, 10)
  | _ -> raise Malformed

(** [cmdToCoordTwo command] is a (char,int) tuple that takes a command of
    type Place and converts it into a valid coordinate that is a (char,int) 
    Raises: [Malformed] if the command is an invalid Place command. *)
let cmdToCoordTwo command = 
  match command with 
  | Place list when (List.length list = 3) -> let coord = List.nth list 2 in 
    if String.length coord = 2 then 
      (String.get coord 0, int_of_char (String.get coord 1) - 48)
    else 
      (String.get coord 0, 10)
  | _ -> raise Malformed

(** [print_text_grid state_p1 state_p2] prints the grid for both the
    states of player 1 and player 2.*)
let print_text_grid state_p1 state_p2 = 
  print_endline ("Player 1's Ships':" ^"\n"^
                 (Textgrid.text_grid 
                    (Textgrid.sort_and_group_rows 
                       (List.rev Battleship.rows) (state_p1.current_grid) []) "")); 
  print_endline ("Player 2's Ships':" ^"\n"^
                 (Textgrid.text_grid (Textgrid.sort_and_group_rows (List.rev Battleship.rows) (state_p2.current_grid) []) "")); ()

(** [play_game_helper state_p1 state_p2 turn] is the helper function for
    playing the actual game. It takes care of placing the ships, changing 
    the grid for when a coordinate is fired at, and ends the game when 
    all ships have been sunk. 
    Raises: [Malformed] if the inputted command is invalid. 
            [OutOfBounds] if the given coordinates is out of bounds. 
            [NotRight] if the given coordinates for placing are equal. 
            [ShipHere] if a ship is already placed at given coordinates*)
let rec play_game_helper state_p1 state_p2 turn =  
  try 
    if (placing state_p1) then 
      (ANSITerminal.
         (print_string [blue] 
            ("To place a ship, type \"place [ship name] [starting coordinate] [ending coordinate]\" 
            \nFor example, \"place carrier a1 a2\"
            \nCarrier has size 2, Destroyer has size 2, Submarine has size 3, Cruiser has size 3, and Battleship has size 4. 
            \nPlayer 1, please place your next ship. Ships remaining: " ^ queue state_p1 ^ "\n\n>"));
       let command = parse (read_line ()) in 
       match command with 
       | Quit -> print_endline "Goodbye!"; exit 0
       | Fire coord -> raise Malformed
       | Status -> raise Malformed
       | Place ship -> 
         let ship = cmdToShip command in 
         let coordOne = cmdToCoordOne command in 
         let coordTwo = cmdToCoordTwo command in 
         print_text_grid state_p1 state_p2;
         play_game_helper (place ship coordOne coordTwo state_p1) state_p2 turn)
    else if (placing state_p2) then
      (ANSITerminal.
         (print_string [blue] 
            ("To place a ship, type \"place [ship name] [starting coordinate] [ending coordinate]\" 
            \nFor example, \"place carrier a1 a2\"
            \nCarrier has size 2, Destroyer has size 2, Submarine has size 3, Cruiser has size 3, and Battleship has size 4. 
            \nPlayer 2, please place your next ship. Ships remaining: " ^ queue state_p2 ^ "\n\n>"));
       let command = parse (read_line ()) in 
       match command with 
       | Quit -> print_endline "Goodbye!"; exit 0
       | Fire coord -> raise Malformed
       | Status -> raise Malformed
       | Place ship -> 
         let ship = cmdToShip command in 
         let coordOne = cmdToCoordOne command in 
         let coordTwo = cmdToCoordTwo command in 
         print_text_grid state_p1 state_p2;
         play_game_helper state_p1 (place ship coordOne coordTwo state_p2) turn)
    else 
      (ANSITerminal.
         (print_string [blue] 
            ("\n The game has now started. \nTo fire, type \"fire [coordinate]\" \nTo see how many ships you have sunk, type \"status\"" 
             ^ if turn then "\n Player 1, make a move.\n >"
             else "\n Player 2, make a move.\n >"))); 

    let userInput  = parse (read_line ()) in
    match userInput with 
    | Fire coord -> 
      let new_state = fire (cmdToTupleFire userInput) 
          (if turn then state_p2 else state_p1) in 
      (if turn && new_state = state_p2 
       then (print_text_grid state_p1 state_p2; 
             print_endline "\n Nothing has happened. Try again.";
             play_game_helper state_p1 state_p2 turn)
       else if turn 
       then (print_text_grid state_p1 new_state; 
             print_endline "Successful fire"; 
             if winOrNot new_state.sunk_list 
             then (print_endline "Player 1 has won."; exit 0)
             else play_game_helper state_p1 new_state (not turn))
       else if new_state = state_p1 
       then (print_text_grid state_p1 state_p2; 
             print_endline "\n Nothing has happened. Try again.";
             play_game_helper state_p1 state_p2 turn)
       else print_text_grid new_state state_p2; 
       print_endline "Successful fire";
       if winOrNot new_state.sunk_list 
       then (print_endline "Player 2 has won."; exit 0)
       else play_game_helper new_state state_p2 (not turn))
    | Status -> print_endline 
                  ("You have sunk: " ^ 
                   (string_of_int (if turn 
                                   then getAmountSunk state_p2.sunk_list 0 
                                   else getAmountSunk state_p1.sunk_list 0)));
      play_game_helper state_p1 state_p2 turn
    | Quit -> print_endline "Goodbye!"; exit 0
    | Place ship-> print_endline "\n All ships have already been placed";
      play_game_helper state_p1 state_p2 turn  

  with 
  | Malformed -> print_endline "\n That was not a valid command.\n";
    play_game_helper state_p1 state_p2 turn
  | OutOfBounds -> print_endline "\n These coordinates are out of bound. \n";
    play_game_helper state_p1 state_p2 turn
  | NotRight -> print_endline "\n These coordinates are equal. \n";
    play_game_helper state_p1 state_p2 turn
  | ShipHere -> print_endline "\n A ship is already placed here.\n";
    play_game_helper state_p1 state_p2 turn



let play_game start = 
  play_game_helper init_state init_state true


let main () = 
  ANSITerminal.print_string [ANSITerminal.Foreground Blue] "\n Battleship\n";
  print_endline "\n Please type start to play a new game.\n ";
  print_string ">>>";
  let start = read_line () in play_game start 

(** Executes the game engine. *)
let () = main ()