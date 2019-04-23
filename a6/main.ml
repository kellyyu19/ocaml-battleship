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
  | Bomb list when (List.length list = 1) -> let coord = List.nth list 0 in 
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
let print_text_grid state_p1 state_p2 ship_vis1 ship_vis2= 
  (ANSITerminal.
     (print_string [on_blue] ("\nPlayer 1's Ships':" ^"\n"^
                              (Textgrid.text_grid 
                                 (Textgrid.sort_and_group_rows 
                                    (List.rev Battleship.rows) (state_p1.current_grid) []) "" ship_vis1)^"\n\n"))); 
  (ANSITerminal.                               
     (print_string [on_red] ("\nPlayer 2's Ships':" ^"\n"^
                             (Textgrid.text_grid (Textgrid.sort_and_group_rows (List.rev Battleship.rows) (state_p2.current_grid) []) "" ship_vis2)^"\n\n"))); ()


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
         (print_string [default] 
            ("To place a ship, type \"place [ship name] [starting coordinate] [ending coordinate]\" 
            \nFor example, \"place carrier a1 a2\"
            \nCarrier has size 2, Destroyer has size 2, Submarine has size 3, Cruiser has size 3, and Battleship has size 4. 
            \nIf you want to place randomly, type \"random\", (you must use this before placing any ships.)
            \nPlayer 1, please place your next ship. Ships remaining: " ^ queue state_p1 ^ "\n\n>"));
       let command = parse (read_line ()) in 
       match command with 
       | Quit -> print_endline "Goodbye!"; exit 0
       | Fire coord -> raise Malformed
       | Status -> raise Malformed
       | PlaceRandom -> Random.init (int_of_float ((Unix.time ())) mod 10000);
         if (List.length state_p1.ships_on_grid )>0 then raise Malformed else 
           let new_state = state_builder_AI state_p1 state_p1.ship_list in 
           print_text_grid new_state state_p2 true false; 
           print_endline "\n\n\n\n\n\n\n\n\n\n";
           print_text_grid new_state state_p2 false false; 

           play_game_helper new_state state_p2 turn
       | Place ship -> 
         let ship = cmdToShip command in 
         let coordOne = cmdToCoordOne command in 
         let coordTwo = cmdToCoordTwo command in 
         let safeCoords = sort_tuple (coordOne,coordTwo) in
         let new_state = (place ship (fst safeCoords) (snd safeCoords) state_p1) in
         print_text_grid new_state state_p2 true false;
         print_endline "\n\n\n\n\n\n\n\n\n\n";
         print_text_grid new_state state_p2 false false;
         play_game_helper new_state state_p2 turn
       | _ -> raise Malformed)

    else if (placing state_p2) then
      (ANSITerminal.
         (print_string [default] 
            ("To place a ship, type \"place [ship name] [starting coordinate] [ending coordinate]\" 
            \nFor example, \"place carrier a1 a2\"
            \nCarrier has size 2, Destroyer has size 2, Submarine has size 3, Cruiser has size 3, and Battleship has size 4.
            \nIf you want to place randomly, type \"random\", (you must use this before placing any ships.)
            \nPlayer 2, please place your next ship. Ships remaining: " ^ queue state_p2 ^ "\n\n>"));
       let command = parse (read_line ()) in 
       match command with 
       | Quit -> print_endline "Goodbye!"; exit 0
       | Fire coord -> raise Malformed
       | Status -> raise Malformed
       | PlaceRandom -> Random.init (int_of_float ((Unix.time ())) mod 10000);
         if (List.length state_p2.ships_on_grid )>0 then raise Malformed else 
           let new_state = state_builder_AI state_p2 state_p2.ship_list in 
           print_text_grid state_p1 new_state false true;
           print_endline "\n\n\n\n\n\n\n\n\n\n";
           print_text_grid state_p1 new_state false false; 
           play_game_helper state_p1 new_state turn
       | Place ship -> 
         let ship = cmdToShip command in 
         let coordOne = cmdToCoordOne command in 
         let coordTwo = cmdToCoordTwo command in 
         let safeCoords = sort_tuple (coordOne,coordTwo) in
         let new_state = (place ship (fst safeCoords) (snd safeCoords) state_p2) in
         print_text_grid state_p1 new_state false true;
         print_endline "\n\n\n\n\n\n\n\n\n\n";
         print_text_grid state_p1 new_state false false;
         play_game_helper state_p1 new_state turn
       | _ -> raise Malformed)

    else 
      (ANSITerminal.
         (print_string [default] 
            ("\n The game has now started. \nTo fire, type \"fire [coordinate]\""^
             "\nTo use a bomb, type \"bomb [coordinate]\" \nTo see how many ships you have sunk, type \"status\"" 
             ^ if turn then "\n Player 1, make a move.\n >"
             else "\n Player 2, make a move.\n >"))); 

    let userInput  = parse (read_line ()) in
    match userInput with 
    | Fire coord -> 
      let new_state = fire (cmdToTupleFire userInput) 
          (if turn then state_p2 else state_p1) in 
      (if turn && new_state = state_p2 
       then (print_text_grid state_p1 state_p2 false false; 
             print_endline "\n Nothing has happened. Try again.";
             play_game_helper state_p1 state_p2 turn)
       else if turn 
       then (print_text_grid state_p1 new_state false false; 
             print_endline "\n Shot fired."; 
             if winOrNot new_state.sunk_list 
             then (print_endline "\n Player 1 has won."; exit 0)
             else play_game_helper state_p1 new_state (not turn))
       else if new_state = state_p1 
       then (print_text_grid state_p1 state_p2 false false; 
             print_endline "\n Nothing has happened. Try again.";
             play_game_helper state_p1 state_p2 turn)
       else print_text_grid new_state state_p2 false false; 
       print_endline "\n Shot fired.";
       if winOrNot new_state.sunk_list 
       then (print_endline "\n Player 2 has won."; exit 0)
       else play_game_helper new_state state_p2 (not turn))
    | Status -> print_endline 
                  ("\n You have sunk: " ^ 
                   (string_of_int (if turn 
                                   then getAmountSunk state_p2.sunk_list 0 
                                   else getAmountSunk state_p1.sunk_list 0)));
      play_game_helper state_p1 state_p2 turn
    | Quit -> print_endline "Goodbye!"; exit 0
    | Place ship-> print_endline "\n All ships have already been placed";
      play_game_helper state_p1 state_p2 turn  
    | Bomb coord -> 
      if can_bomb (if turn then state_p2 else state_p1)then 
        let new_state = bomb (cmdToTupleFire userInput) 
            (if turn then state_p2 else state_p1) in 
        (if turn && {new_state with bombs_left=state_p2.bombs_left} = state_p2 
         then (print_text_grid state_p1 state_p2 false false; 
               print_endline "\n Nothing has happened. Try again.";
               play_game_helper state_p1 state_p2 turn)
         else if turn 
         then (print_text_grid state_p1 new_state false false; 
               print_endline "\n Bomb fired."; 
               if winOrNot new_state.sunk_list 
               then (print_endline "Player 1 has won."; exit 0)
               else play_game_helper state_p1 new_state (not turn))
         else if {new_state with bombs_left=state_p1.bombs_left} = state_p1 
         then (print_text_grid state_p1 state_p2 false false; 
               print_endline "\n Nothing has happened. Try again.";
               play_game_helper state_p1 state_p2 turn)
         else print_text_grid new_state state_p2 false false; 
         print_endline "\n Bomb fired.";
         if winOrNot new_state.sunk_list 
         then (print_endline "\n Player 2 has won."; exit 0)
         else play_game_helper new_state state_p2 (not turn))
      else (print_endline "\n You have no more bombs. Enter another command."); 
      if turn then 
        play_game_helper state_p1 state_p2 turn 
      else play_game_helper state_p2 state_p1 (not turn) 
    | _ -> raise Malformed

  with 
  | Malformed -> print_endline "\n That was not a valid command.\n";
    play_game_helper state_p1 state_p2 turn
  | OutOfBounds -> print_endline "\n These coordinates are out of bound. \n";
    play_game_helper state_p1 state_p2 turn
  | NotRight -> print_endline "\n These coordinates are equal. \n";
    play_game_helper state_p1 state_p2 turn
  | ShipHere -> print_endline "\n A ship is already placed here.\n";
    play_game_helper state_p1 state_p2 turn
  | Empty -> print_endline "\n That was not a valid command.";
    play_game_helper state_p1 state_p2 turn
  | Invalid_argument some -> print_endline "\n That was not a valid command. \n";
    play_game_helper state_p1 state_p2 turn

let rec solo_game_helper state_p1 state_AI = 
  try
    if (placing state_p1) then 
      (ANSITerminal.
         (print_string [default] 
            ("If you want to place all the ships randomly, type \"random\" , (you must use this before placing any ships.)
            \nTo place a ship, type \"place [ship name] [starting coordinate] [ending coordinate]\" 
            \nFor example, \"place carrier a1 a2\"
            \nCarrier has size 2, Destroyer has size 2, Submarine has size 3, Cruiser has size 3, and Battleship has size 4. 
            \nPlayer 1, please place your next ship. Ships remaining: " ^ queue state_p1 ^ "\n\n>"));
       let command = parse (read_line ()) in 
       match command with 
       | Quit -> print_endline "Goodbye!"; exit 0
       | Fire coord -> raise Malformed
       | Status -> raise Malformed
       | PlaceRandom ->  Random.init (int_of_float ((Unix.time ())) mod 10000);
         if (List.length state_p1.ships_on_grid )>0 then raise Malformed else 
           let new_state = state_builder_AI state_p1 state_p1.ship_list in 
           print_text_grid new_state state_AI true false;
           solo_game_helper new_state state_AI
       | Place ship -> 
         let ship = cmdToShip command in 
         let coordOne = cmdToCoordOne command in 
         let coordTwo = cmdToCoordTwo command in 
         let safeCoords = sort_tuple (coordOne,coordTwo) in
         let new_state = (place ship (fst safeCoords) (snd safeCoords) state_p1) in
         print_text_grid new_state state_AI true false;
         solo_game_helper new_state state_AI
       | _ -> raise Malformed)
    else 
      (ANSITerminal.
         (print_string [default] 
            ("\n The game has now started. \nTo fire, type \"fire [coordinate]"^
             "\" \nTo use a bomb, type \"bomb [coordinate]\" \nTo see how many ships you have sunk, type \"status\"" 
             ^ "\n Player 1, make a move.\n >")); 

       let userInput  = parse (read_line ()) in
       match userInput with 
       | Fire coord -> 
         let new_state = fire (cmdToTupleFire userInput) state_AI  in 
         if (new_state = state_AI) 
         then (print_text_grid state_p1 state_AI true false; 
               print_endline "\n Nothing has happened. Try again.";
               solo_game_helper state_p1 state_AI)
         else 
           (print_endline "\n Shot fired."; 
            if winOrNot new_state.sunk_list 
            then (print_endline "\n Player 1 has won."; exit 0)
            else
              let rec ai_fire_helper state_p1 state_AI = 
                let decider = (state_AI.bombs_left > 0) && ((Random.int (int_of_float ((Unix.time ())) mod 10000)) mod 2)=0 in
                let new_state = 
                  if decider then
                    fire (fire_AI_coords state_p1.current_grid state_p1.current_grid) state_p1
                  else bomb (fire_AI_coords state_p1.current_grid state_p1.current_grid) state_p1 in
                if (new_state = state_p1) 
                then ai_fire_helper state_p1 state_AI
                else (print_text_grid new_state state_AI true false; new_state) in 
              let new_p1 = ai_fire_helper state_p1 new_state in 
              if winOrNot new_p1.sunk_list 
              then (print_endline "\n The AI has won."; exit 0)
              else solo_game_helper new_p1 new_state)
       | Bomb coord -> 
         if can_bomb state_AI then 
           let new_state = bomb (cmdToTupleFire userInput) state_AI in 
           if ({new_state with bombs_left=state_AI.bombs_left} = state_AI)
           then (print_text_grid state_p1 state_AI true false; 
                 print_endline "\n Nothing has happened. Try again.";
                 solo_game_helper state_p1 state_AI)
           else 
             (print_endline "\n Bomb fired."; 
              if winOrNot new_state.sunk_list 
              then (print_endline "\n Player 1 has won."; exit 0)
              else 
                let rec ai_fire_helper state_p1 state_AI = 
                  let new_state = fire (fire_AI_coords state_p1.current_grid state_p1.current_grid) state_p1 in
                  if (new_state = state_p1) 
                  then ai_fire_helper state_p1 state_AI
                  else (print_text_grid new_state state_AI true false; new_state) in 
                let new_p1 = ai_fire_helper state_p1 new_state in 
                if winOrNot new_p1.sunk_list 
                then (print_endline "\n The AI has won."; exit 0)
                else solo_game_helper new_p1 new_state)
         else (print_endline "\n You have no more bombs. Enter another command."); 
         solo_game_helper state_p1 state_AI
       | Status -> 
         print_endline 
           ("You have sunk: " ^ 
            (string_of_int (getAmountSunk state_AI.sunk_list 0)) ^  
            "\nYou have lost " ^ 
            (string_of_int (getAmountSunk state_p1.sunk_list 0)) ^ " ships.\n");
         solo_game_helper state_p1 state_AI 
       | Quit -> print_endline "Goodbye!"; exit 0
       | Place ship-> print_endline "\n All ships have already been placed.";
         solo_game_helper state_p1 state_AI 
       | _ -> raise Malformed)

  with 
  | Malformed -> print_endline "\n That was not a valid command.\n";
    solo_game_helper state_p1 state_AI
  | OutOfBounds -> print_endline "\n These coordinates are out of bound. \n";
    solo_game_helper state_p1 state_AI
  | NotRight -> print_endline "\n These coordinates can not form a ship. \n";
    solo_game_helper state_p1 state_AI
  | ShipHere -> print_endline "\n A ship is already placed here.\n";
    solo_game_helper state_p1 state_AI
  | Empty -> print_endline "\n That was not a valid command. \n";
    solo_game_helper state_p1 state_AI
  | Invalid_argument some -> print_endline "\n That was not a valid command. \n";
    solo_game_helper state_p1 state_AI


let rec play_game mode = 
  try
    match parse mode with
    | Versus -> play_game_helper init_state init_state true
    | Solo -> let fresh_state = init_state in 
      let ai_state = state_builder_AI fresh_state fresh_state.ship_list in 
      solo_game_helper init_state ai_state
    | Quit -> print_endline "Goodbye!"; exit 0
    | _ -> raise Malformed
  with 
  | Malformed -> print_endline 
                   "\n Please enter a valid gamemode.\n ~Versus\n ~Solo\n";
    print_string ">>>";
    play_game ((read_line ()))
  | Empty -> print_endline 
               "\n Please enter a valid gamemode.\n ~Versus\n ~Solo\n";
    print_string ">>>";
    play_game ((read_line ()))


let main () = 
  ANSITerminal.print_string [ANSITerminal.Foreground Blue] "\n Battleship\n";
  print_endline "\n Choose a game mode \n\n ~Versus\n ~Solo ";
  print_string ">>>";
  let mode = (read_line ()) in play_game mode 

(** Executes the game engine. *)
let () = main ()