open Battleship
open State
open Command








let rec play_game_helper state =  


  let play_game start = 
    play_game_helper init_state


let main () = 
  ANSITerminal.print_string [ANSITerminal.Foreground Blue] "\n Battleship\n";
  print_endline "\n Please type start to play a new game.\n ";
  print_string ">>>";
  let start = read_line () in play_game start


let () = main ()