open Battleship
open State
open Command












let main () = 
  ANSITerminal.print_string [ANSITerminal.Foreground Blue] "Test\n";
  print_string ">>>";
  match read_line () with 
  |_ -> print_endline "unimplemented"

let () = main ()