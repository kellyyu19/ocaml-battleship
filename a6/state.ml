open Battleship

type state = {ship_list: ship list; current_grid: grid; sunk_list: ship list; 
              ships_on_grid: ship list}

let init_battleship = {name = Battleship; size = 4; hits = 0}

let init_cruiser = {name = Cruiser; size = 3; hits = 0}

let init_submarine = {name = Submarine; size = 3; hits = 0}

let init_destroyer = {name = Destroyer; size = 2; hits = 0}

let init_carrier = {name = Carrier; size = 2; hits = 0}

let init_ships = 
  [{name = Battleship; size = 4; hits = 0}; {name = Cruiser; size = 3; hits = 0}; 
   {name = Submarine; size = 3; hits = 0}; {name = Destroyer; size = 2; hits = 0};
   {name = Carrier; size = 2; hits = 0}] 

let init_state : state = {ship_list = init_ships; 
                          current_grid = Battleship.init_grid Battleship.rows Battleship.columns [];
                          sunk_list = [];
                          ships_on_grid = []}

exception OutOfBounds 
exception NotRight

let place (ship:ship) (coordOne:coordinate) (coordTwo:coordinate) (state:state) = 
  if not ((fst coordOne = fst coordTwo) || (snd coordOne = snd coordTwo)
          || (fst coordOne = fst coordTwo && snd coordOne = snd coordTwo)) 
  then raise NotRight
  else if (snd coordOne = snd coordTwo && Pervasives.abs (Char.code (fst coordOne) 
                                                          - Char.code(fst coordTwo)) = ship.size - 1 ) 
  then 
    let coords = Battleship.make_new_char_list rows (snd coordOne) (fst coordOne) (fst coordTwo) [] in 
    {ship_list=init_ships; current_grid = Battleship.make_grid ship coords state.current_grid [];
     sunk_list=[]; ships_on_grid=ship::state.ships_on_grid}
  else if (fst coordOne = fst coordTwo && Pervasives.abs (snd coordOne - snd coordTwo) = ship.size - 1)
  then
    let coords = Battleship.make_new_int_list (fst coordOne) columns (snd coordOne) (snd coordTwo) [] in 
    {ship_list=init_ships; current_grid = Battleship.make_grid ship coords state.current_grid [];
     sunk_list=[]; ships_on_grid=ship::state.ships_on_grid}
  else raise OutOfBounds

let rec new_ship_list ship ship_list outlist : ship list= print_endline "line 46";
  match ship_list with 
  | [] -> outlist
  | {name=nm;size=sz;hits=hts}::t when nm=ship.name -> new_ship_list ship t ({name=ship.name;
                                                                              size=ship.size; hits=hts+1}::outlist)
  | h::t -> new_ship_list ship t (h::outlist)

let rec sink_ship ship (currentGrid:Battleship.grid) outlist = 
  match currentGrid with 
  |[] -> outlist 
  |((r,c),s)::t when s = Hit(ship) || s = Occupied(ship)  -> print_endline "line 56";
    sink_ship ship t (((r,c),Sunk(ship))::outlist)
  |((r,c),s)::t -> sink_ship ship t (((r,c),s)::outlist)


let rec update_grid_occupied ship coord (currentGrid:Battleship.grid) outlist = 
  match currentGrid with 
  | [] -> outlist
  | ((r,c),s)::t  -> if (r,c) = coord 
    then 
      let new_ship = {ship with hits = ship.hits+1} in
      if new_ship.hits = new_ship.size then sink_ship ship currentGrid []
      else (((r,c),Hit(new_ship))::outlist) @ t 
    else update_grid_occupied ship coord t (((r,c),s)::outlist)

let rec update_grid_empty coord (currentGrid:Battleship.grid) outlist =
  match currentGrid with
  | [] -> outlist
  | ((r,c),s)::t  -> if (r,c) = coord then (((r,c),Miss)::outlist) @ t
    else update_grid_empty coord t (((r,c),s)::outlist)

let is_sunk ship : bool =
  if ship.hits>=ship.size then (print_endline "sunk"; true) else (print_endline ("not sink, hits and size are " ^ string_of_int ship.hits ^ "," ^ string_of_int ship.size); false)

let rec curr_sunk_list currShipList outlist = 
  match currShipList with 
  | [] -> print_endline "empty currShipList"; outlist 
  | h::t -> if (is_sunk h) then curr_sunk_list t (h::outlist) 
    else curr_sunk_list t outlist

let fire (coord: coordinate) (currentState: state) =
  let rec fireHelper coord currGrid currShipList= 
    match currGrid with 
    | [] ->  {ship_list = currShipList; current_grid = currGrid; 
              sunk_list = curr_sunk_list currShipList [];
              ships_on_grid = currentState.ships_on_grid}
    | ((r,c),Empty)::t when (r,c) = coord -> 
      let update_grid_var = update_grid_empty coord currGrid [] in 
      {ship_list=currShipList; current_grid=update_grid_var;
       sunk_list = currentState.sunk_list;
       ships_on_grid = currentState.ships_on_grid}
    | ((r,c),Hit(s))::t when (r,c)=coord -> {ship_list = currShipList; current_grid = currGrid; 
                                             sunk_list = curr_sunk_list currShipList [];
                                             ships_on_grid = currentState.ships_on_grid}
    | ((r,c),Miss)::t when (r,c)=coord -> {ship_list = currShipList; current_grid = currGrid; 
                                           sunk_list = curr_sunk_list currShipList [];
                                           ships_on_grid = currentState.ships_on_grid}
    | ((r,c),Occupied(s))::t when (r,c)=coord -> 
      let update_ship_list = new_ship_list s currShipList [] in 
      let update_grid_var = update_grid_occupied s coord currGrid [] in 
      {ship_list=update_ship_list; current_grid=update_grid_var;
       sunk_list = curr_sunk_list update_ship_list [];
       ships_on_grid = currentState.ships_on_grid}
    | ((r,c),point)::t -> let new_state = fireHelper coord t currShipList in 
      let new_grid = ((r,c),point)::new_state.current_grid in 
      { new_state with current_grid = new_grid}
  in fireHelper coord currentState.current_grid currentState.ship_list 

let placing currentState : bool = 
  if List.length currentState.ships_on_grid <> 5 then true else false

let string_of_ships ship = 
  match ship.name with 
  | Carrier -> "Carrier"
  | Battleship -> "Battleship"
  | Cruiser -> "Cruiser"
  | Submarine -> "Submarine"
  | Destroyer -> "Destroyer"

let rec queue_helper currentState initships outlist : ship list = 
  let curr_ships_on_grid = currentState.ships_on_grid in 
  match initships with 
  | [] -> outlist 
  | h::t -> if List.mem h curr_ships_on_grid then queue_helper currentState t outlist 
    else queue_helper currentState t (h::outlist)

let queue currentState = 
  let ships_left = queue_helper currentState init_ships [] in 
  let ship_name ship = string_of_ships ship in 
  let concat a b = 
    if a="" then b else (a ^ ", " ^ b) in
  let ships_left_names = List.map ship_name ships_left in
  List.fold_left concat "" ships_left_names

let rec getAmountSunk lst accum = 
  match lst with
  | [] -> accum
  | h::t -> getAmountSunk t (accum + 1) 

let winOrNot lst : bool = 
  if List.length lst = 5 then true else false
