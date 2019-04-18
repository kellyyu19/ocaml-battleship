open Battleship 
open Command
open State 
open Textgrid

let start_state = init_state 
let carrier_placed = place (init_carrier) ('a',1) ('a',2) start_state
let destroyer_placed = place (init_destroyer) ('b',1) ('b',2) carrier_placed
let submarine_placed = place (init_submarine) ('c',1) ('c',3) destroyer_placed
let cruiser_placed = place (init_cruiser) ('d',1) ('d',3) submarine_placed
let battleship_placed = place (init_battleship) ('e',1) ('e',4) cruiser_placed
let carrier_fired1 = fire ('a',1) battleship_placed
let state_tests = 
  [

    











  ]

