open Battleship
open State
open Command




let cmdToTupleFire command =
  match command with
  | Fire list when (List.length list = 1) -> let coord = List.nth list 0 in 
    (String.get coord 0,int_of_char (String.get coord 1))
  | _ -> raise Malformed



let rec play_game_helper state_p1 state_p2 turn =  
  if (placing state_p1) then 
    ANSITerminal.print_string [ANSITerminal.Foreground Blue] 
      "\n Player 1, please place your next ship. Ships remaining: " ^ queue state_p1;
  play_game_helper (place state_p1) state_p2
else if (placing state_p2) then
  ANSITerminal.print_string [ANSITerminal.Foreground Blue] 
    "\n Player 2, please place your next ship. Ships remaining: " ^ queue state_p1;
play_game_helper (place state_p1) state_p2
else 
  try 
    let userInput  = shave read_line () in
    match userInput with 
    | Fire coord -> fire cmdToTupleFire (if turn then state_p1 else state_p2)
    | Status ->
    | Quit -> 
  with 
  | Malformed -> print_endline "\n That was not a valid command.\n";
    play_game_helper state_p1 state_p2



let play_game start = 
  play_game_helper init_state init_state


let main () = 
  ANSITerminal.print_string [ANSITerminal.Foreground Blue] "\n Battleship\n";
  print_endline "\n Please type start to play a new game.\n ";
  print_string ">>>";
  let start = read_line () in play_game start


let () = main ()