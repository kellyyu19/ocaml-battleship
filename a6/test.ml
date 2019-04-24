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
let bomb_at_a1 = bomb ('a',1) battleship_placed

let state_tests = 
  [
    "testFireAtCarrier" >:: (fun _ -> assert_equal (('a', 1), Hit {name = Carrier; size = 2; hits = 1})
                                (get_point ('a', 1) carrier_fired1.current_grid)) ;
    "testFireAtMiddleOfBattleship" >:: (fun _ -> assert_equal (('e', 3), Hit {name = Battleship; size = 4; hits = 1})
                                           (get_point ('e', 3) battleship_fired.current_grid) );
    "testEmptyPoint" >:: (fun _ -> assert_equal (('f', 10), Miss)
                             (get_point ('f', 10) missed_fire.current_grid) );
    "testSameEmptyPoint" >:: (fun _ -> assert_equal (missed_fire.current_grid)
                                 (fire_at_same_missed_point.current_grid) );
    "testSameHitPoint" >:: (fun _ -> assert_equal (fire_at_same_hit_point.current_grid)
                               (fire_at_same_missed_point.current_grid) );
    "TestVerticalPlacement@a1" >:: (fun _ -> assert_equal (('a',1), Occupied {name = Carrier; size = 2; hits = 0})
                                       (get_point ('a',1) carrier_placed.current_grid) );
    "TestVerticalPlacement@b1" >:: (fun _ -> assert_equal (('b',1), Occupied {name = Carrier; size = 2; hits = 0})
                                       (get_point ('b',1) carrier_placed.current_grid) );
    "TestHorizontalPlacement@b2" >:: (fun _ -> assert_equal (('b',2), Occupied {name = Destroyer; size = 2; hits = 0})
                                         (get_point ('b',2) destroyer_placed.current_grid) );
    "TestVerticalPlacement@b3" >:: (fun _ -> assert_equal (('b',3), Occupied {name = Destroyer; size = 2; hits = 0})
                                       (get_point ('b',3) destroyer_placed.current_grid) );
    "TestPlacedOutOfBoundsChar" >:: (fun _ -> assert_raises(OutOfBounds) 
                                        (fun () -> place (init_carrier) ('z',1) ('z',2) start_state));
    "TestPlacedOutOfBoundsNum" >:: (fun _ -> assert_raises(OutOfBounds) 
                                       (fun () -> place (init_carrier) ('a',10) ('a',11) start_state));
    "TestPlacedWrongLengthCarrier" >:: (fun _ -> assert_raises(OutOfBounds) 
                                           (fun () -> place (init_carrier) ('a',1) ('a',3) start_state));
    "TestPlacedWrongLengthCruiser" >:: (fun _ -> assert_raises(OutOfBounds) 
                                           (fun () -> place (init_cruiser) ('a',1) ('a',9) start_state));
    "TestPlacedNotRightCruiser" >:: (fun _ -> assert_raises(NotRight) 
                                        (fun () -> place (init_cruiser) ('a',1) ('b',9) start_state));
    "TestPlacedNotRightCarrier" >:: (fun _ -> assert_raises(NotRight) 
                                        (fun () -> place (init_carrier) ('a',1) ('c',2) start_state));

  ]
let make_command_tests  
    (name : string) 
    (input: string) 
    (expected_output: command) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (parse input))
let command_tests = 
  [ make_command_tests "testquit1" "quit" Quit;
    make_command_tests "testquit2" "         quit    " Quit;
    make_command_tests "testlegal1" "fire a1 " (Fire["a1"]);
    make_command_tests "testlegal2" 
      "  fire   a1           " (Fire["a1"]);
    make_command_tests "testlegal3" 
      "  fire   b1           " (Fire["b1"]);
    make_command_tests "teststatus1" 
      "  status           " (Status);
    make_command_tests "teststatus2" 
      "status" (Status);
    make_command_tests "testlegal4" 
      "  place carrier   a1     a2      " (Place["carrier"; "a1"; "a2"]);
    make_command_tests "testlegal5" 
      "place cruiser b1 b3" (Place["cruiser"; "b1"; "b3"]);

    "testempty1" >:: (fun _ -> assert_raises(Empty) (fun () -> parse ("") ));
    "testempty2" >:: (fun _ -> assert_raises(Empty)
                         (fun () -> parse ("      ")));
    "testmalformed1" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("run      ") ));
    "testmalformed2" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("run a1 ") ));
    "testmalformed3" >:: (fun _ -> assert_raises(Malformed)
                             (fun () -> parse ("quit a1") ));
    "testmalformed4" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("fire       ") ));
    "testmalformed5" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("status battleship       ") ));
    "testmalformed6" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("inventory chocolate room  ") ));
    "testmalformed7" >:: (fun _ -> assert_raises(Malformed) 
                             (fun () -> parse ("place       ") ));
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    state_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite

