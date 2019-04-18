open OUnit2
open Battleship 
open Command
open State 
open Textgrid

let rec get_point (coord:Battleship.coordinate) (grid: Battleship.grid) = 
  match grid with 
  |[] -> failwith"coordinate is not in this grid, something is wrong"
  |h::t -> if fst(h) = coord then h else get_point coord t



let start_state = init_state 
let carrier_placed = place (init_carrier) ('a',1) ('a',2) start_state
let destroyer_placed = place (init_destroyer) ('b',1) ('b',2) carrier_placed
let submarine_placed = place (init_submarine) ('c',1) ('c',3) destroyer_placed
let cruiser_placed = place (init_cruiser) ('d',1) ('d',3) submarine_placed
let battleship_placed = place (init_battleship) ('e',1) ('e',4) cruiser_placed
let carrier_fired1 = fire ('a',1) battleship_placed
let battleship_fired = fire('e', 3) carrier_fired1

let state_tests = 
  [
    "testFireAtCarrier" >:: (fun _ -> assert_equal (('a', 1), Hit {name = Carrier; size = 2; hits = 1})
                                (List.hd (carrier_fired1.current_grid)) );
    "testFireAtBattleship" >:: (fun _ -> assert_equal (('e', 3), Hit {name = Battleship; size = 4; hits = 1})
                                   (get_point ('e', 3) battleship_fired.current_grid) );


















  ]

