open Battleship
open State
open Command








let rec play_game_helper state_p1 state_p2 =  
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