open Battleship

type state = {ship_list: ship list; current_grid: grid}

let init_ships = 
  [{name = Battleship; size = 4; hits = 0}; {name = Cruiser; size = 3; hits = 0}; 
   {name = Submarine; size = 3; hits = 0}; {name = Destroyer; size = 2; hits = 0};
   {name = Destroyer; size = 2; hits = 0}] 

let init_state : state = {ship_list = init_ships; 
                          current_grid = Battleship.init_grid Battleship.rows Battleship.columns []}

let place (ship:ship) (coordOne:coordinate) (coordTwo:coordinate) (grid:grid) = 
  if not ((fst coordOne = fst coordTwo) || (snd coordOne = snd coordTwo)
          || (fst coordOne = fst coordTwo && snd coordOne = snd coordTwo)) 
  then raise (Failure "Invalid Coords")
  else if (snd coordOne = snd coordTwo && Pervasives.abs (Char.code (fst coordOne) 
                                                          - Char.code(fst coordTwo)) = ship.size ) 
  then 
    let coords = Battleship.make_new_char_list rows (snd coordOne) (fst coordOne) (fst coordTwo) [] in 
    Battleship.make_grid ship coords grid []
  else if (fst coordOne = fst coordTwo && Pervasives.abs (snd coordOne - snd coordTwo) = ship.size)
  then 
    let coords = Battleship.make_new_int_list (fst coordOne) columns (snd coordOne) (snd coordTwo) [] in 
    Battleship.make_grid ship coords grid []
  else raise (Failure "Invalid Coords")

let rec fire (coord: coordinate) (grid:grid) (outlist:grid) =
  match grid with 
  | [] -> outlist
  | ((r,c),Occupied(s))::t when (r,c)=coord -> fire coord grid ((r,c),Hit)::outlist)
| 