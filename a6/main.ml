open Battleship
open State
open Command
open Textgrid



let cmdToTupleFire command =
  match command with
  | Fire list when (List.length list = 1) -> let coord = List.nth list 0 in 
    if String.length coord = 2 then 
      (String.get coord 0, int_of_char (String.get coord 1) - 48)
    else 
      (String.get coord 0, 10)
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
    if String.length coord = 2 then 
      (String.get coord 0, int_of_char (String.get coord 1) - 48)
    else 
      (String.get coord 0, 10)
  | _ -> raise Malformed

let cmdToCoordTwo command = 
  match command with 
  | Place list when (List.length list = 3) -> let coord = List.nth list 2 in 
    if String.length coord = 2 then 
      (String.get coord 0, int_of_char (String.get coord 1) - 48)
    else 
      (String.get coord 0, 10)
  | _ -> raise Malformed

let print_text_grid state_p1 state_p2 = 
  print_endline ("Player 1's Targets':" ^"\n"^
                 (Textgrid.text_grid (Textgrid.sort_and_group_rows (List.rev Battleship.rows) (state_p1.current_grid) []) "")); 
  print_endline ("Player 2's Targets':" ^"\n"^
                 (Textgrid.text_grid (Textgrid.sort_and_group_rows (List.rev Battleship.rows) (state_p2.current_grid) []) "")); ()

let rec play_game_helper state_p1 state_p2 turn =  
  try 
    if (placing state_p1) then 
      (ANSITerminal.(print_string [blue] 
                       ("\n Player 1, please place your next ship. Ships remaining: " ^ queue state_p1 ^ "\n>"));
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
      (ANSITerminal.(print_string [blue] 
                       ("\n Player 2, please place your next ship. Ships remaining: " ^ queue state_p2 ^ "\n>"));
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
      (ANSITerminal.(print_string [blue] 
                       ("\n The game has now started." ^ if turn then "\n Player 1, make a move.\n >"
                        else "\n Player 2, make a move.\n >"))); 

    let userInput  = parse (read_line ()) in
    match userInput with 
    | Fire coord -> 
      let new_state = fire (cmdToTupleFire userInput) (if turn then state_p2 else state_p1) in 
      (if turn && new_state = state_p2 then (print_text_grid state_p1 state_p2; print_endline "\n Nothing has happened. ";
                                             play_game_helper state_p1 state_p2 (not turn))
       else if turn then (print_text_grid state_p1 new_state; print_endline "successful fire"; play_game_helper state_p1 new_state (not turn))
       else if new_state = state_p1 then (print_text_grid state_p1 state_p2; print_endline "\n Nothing has happened.";
                                          play_game_helper state_p1 state_p2 (not turn))
       else print_text_grid new_state state_p2; print_endline "successful fire"; play_game_helper new_state state_p2 (not turn))
    | Status -> print_endline ("You have sunk: " ^ 
                               (string_of_int (if turn then getAmountSunk state_p1.sunk_list 0 
                                               else getAmountSunk state_p2.sunk_list 0)));
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


let () = main ()