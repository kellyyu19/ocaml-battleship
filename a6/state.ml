open Battleship

type state = {ship_list: ship list; current_grid: grid; sunk_list: ship list}

let init_ships = 
  [{name = Battleship; size = 4; hits = 0}; {name = Cruiser; size = 3; hits = 0}; 
   {name = Submarine; size = 3; hits = 0}; {name = Destroyer; size = 2; hits = 0};
   {name = Destroyer; size = 2; hits = 0}] 

let init_state : state = {ship_list = init_ships; 
                          current_grid = Battleship.init_grid Battleship.rows Battleship.columns [];
                          sunk_list = []}

let place (ship:ship) (coordOne:coordinate) (coordTwo:coordinate) (state:state) = 
  if not ((fst coordOne = fst coordTwo) || (snd coordOne = snd coordTwo)
          || (fst coordOne = fst coordTwo && snd coordOne = snd coordTwo)) 
  then raise (Failure "Invalid Coords")
  else if (snd coordOne = snd coordTwo && Pervasives.abs (Char.code (fst coordOne) 
                                                          - Char.code(fst coordTwo)) = ship.size ) 
  then 
    let coords = Battleship.make_new_char_list rows (snd coordOne) (fst coordOne) (fst coordTwo) [] in 
    {ship_list=init_ships; current_grid = Battleship.make_grid ship coords state.current_grid [];
     sunk_list=[]}
  else if (fst coordOne = fst coordTwo && Pervasives.abs (snd coordOne - snd coordTwo) = ship.size)
  then 
    let coords = Battleship.make_new_int_list (fst coordOne) columns (snd coordOne) (snd coordTwo) [] in 
    {ship_list=init_ships; current_grid = Battleship.make_grid ship coords state.current_grid [];
     sunk_list=[]}
  else raise (Failure "Invalid Coords")

let rec new_ship_list ship ship_list outlist : ship list= 
  match ship_list with 
  | [] -> outlist
  | h::t when h=ship -> new_ship_list ship t ({name=ship.name; 
                                               size=ship.size; hits=ship.hits+1}::outlist)
  | h::t -> new_ship_list ship t (h::outlist) 

let rec update_grid ship coord (currentGrid:Battleship.grid) outlist = 
  match currentGrid with 
  | [] -> outlist
  | ((r,c),s)::t  -> if (r,c) = coord then update_grid ship coord t (((r,c),Hit(ship))::outlist)
    else update_grid ship coord t outlist

let rec is_sunk ship : bool =
  if ship.hits=ship.size then true else is_sunk ship 

let rec curr_sunk_list currShipList outlist = 
  match currShipList with 
  | [] -> outlist 
  | h::t -> if (is_sunk h) then curr_sunk_list t (h::outlist) 
    else curr_sunk_list t outlist

let fire (coord: coordinate) (currentState: state) =
  let rec fireHelper coord currGrid currShipList= 
    match currGrid with 
    | [] -> {ship_list = currShipList; current_grid = currGrid; 
             sunk_list = curr_sunk_list currShipList []}
    | ((r,c),Occupied(s))::t when (r,c)=coord -> 
      let update_ship_list = new_ship_list s currShipList [] in 
      let update_grid = update_grid s coord currGrid [] in 
      {ship_list=update_ship_list; current_grid=update_grid;
       sunk_list = curr_sunk_list update_ship_list []}
    | ((_,_),_)::t -> fireHelper coord t currShipList
  in fireHelper coord currentState.current_grid currentState.ship_list 