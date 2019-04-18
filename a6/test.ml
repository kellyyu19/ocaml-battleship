open OUnit2
open Battleship 
open Command
open State 
open Textgrid

let rec get_point (coord:Battleship.coordinate) (grid: Battleship.grid) = 
  match grid with 
  |[] -> failwith"coordinate is not in this grid, something is wrong"
  |h::t -> if fst(h) = coord then h else get_point coord t


(*States to build off each other and use in testing*)
let start_state = init_state 
let carrier_placed = place (init_carrier) ('a',1) ('b',1) start_state
let destroyer_placed = place (init_destroyer) ('b',2) ('b',3) carrier_placed
let submarine_placed = place (init_submarine) ('c',1) ('c',3) destroyer_placed
let cruiser_placed = place (init_cruiser) ('d',1) ('d',3) submarine_placed
let battleship_placed = place (init_battleship) ('e',1) ('e',4) cruiser_placed
let carrier_fired1 = fire ('a',1) battleship_placed
let battleship_fired = fire('e', 3) carrier_fired1
let missed_fire = fire ('f', 10) battleship_fired
let fire_at_same_missed_point = fire ('f', 10) missed_fire
let fire_at_same_hit_point = fire ('e', 3) fire_at_same_missed_point

let state_tests = 
  [
    (*fire*)
    "testFireAtCarrier" >:: (fun _ -> assert_equal (('a', 1), Hit {name = Carrier; size = 2; hits = 1})
                                (List.hd (carrier_fired1.current_grid)) );
    "testFireAtBattleship" >:: (fun _ -> assert_equal (('e', 3), Hit {name = Battleship; size = 4; hits = 1})
                                   (get_point ('e', 3) battleship_fired.current_grid) );
    "testEmptyPoint" >:: (fun _ -> assert_equal (('f', 10), Miss)
                             (get_point ('f', 10) missed_fire.current_grid) );
    "testSameEmptyPoint" >:: (fun _ -> assert_equal (missed_fire.current_grid)
                                 (fire_at_same_missed_point.current_grid) );
    "testSameHitPoint" >:: (fun _ -> assert_equal (fire_at_same_hit_point.current_grid)
                               (*incorrect place*)

                               (fire_at_same_missed_point.current_grid) );
  ]


let suite =
  "test suite for A2"  >::: List.flatten [
    state_tests;
  ]

let _ = run_test_tt_main suite

