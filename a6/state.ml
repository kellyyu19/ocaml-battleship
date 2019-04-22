open Battleship

(**  An instance of a battleship game. *)
type state = {ship_list: ship list; current_grid: grid; sunk_list: ship list; 
              ships_on_grid: ship list}

(**  [init_battleship] is a new battleship *)
let init_battleship = {name = Battleship; size = 4; hits = 0}

(**  [init_cruiser] is a new cruiser *)
let init_cruiser = {name = Cruiser; size = 3; hits = 0}

(**  [init_submarine] is a new submarine *)
let init_submarine = {name = Submarine; size = 3; hits = 0}

(**  [init_destroyer] is a new destroyer *)
let init_destroyer = {name = Destroyer; size = 2; hits = 0}

(**  [init_carrier] is a new carrier *)
let init_carrier = {name = Carrier; size = 2; hits = 0}

(**  [init_ships] is a list containing one of each type of ship, with hits set to 0. *)
let init_ships = 
  [{name = Battleship; size = 4; hits = 0}; {name = Cruiser; size = 3; hits = 0}; 
   {name = Submarine; size = 3; hits = 0}; {name = Destroyer; size = 2; hits = 0};
   {name = Carrier; size = 2; hits = 0}] 

(** [init_state] is a new state, i.e. a new battleship game. *)
let init_state : state = {ship_list = init_ships; 
                          current_grid = Battleship.init_grid Battleship.rows 
                              Battleship.columns [];
                          sunk_list = [];
                          ships_on_grid = []}

(** OutOfBounds is raised when the given coordinates are outside of the grid or do not 
    correspond to ship size. *)
exception OutOfBounds 

(** NotRight is raised when the given coordinates could not possibly make a  ship 
    because they are neither part of the same column nor the same row. *)
exception NotRight

(** [place ship coordOne coordTwo state] is the new state of the game when 
    [ship] is placed in the grid given by [state]. 
    Raises: NotRight is coordinates are incompatible. 
            OutOfBounds if coordinates are outside of the grid. *)

let place (ship:ship) (coordOne:coordinate) (coordTwo:coordinate) (state:state) = 
  if (fst(coordOne) > 'j' || snd(coordOne) > 10) || 
     (fst(coordTwo) > 'j' || snd(coordTwo) > 10) then raise OutOfBounds 

  else if not ((fst coordOne = fst coordTwo) || (snd coordOne = snd coordTwo)
               || (fst coordOne = fst coordTwo && snd coordOne = snd coordTwo)) 
  then raise NotRight
  else if (snd coordOne = snd coordTwo && 
           Pervasives.abs (Char.code (fst coordOne) 
                           - Char.code(fst coordTwo)) = ship.size - 1 ) 
  then 
    let coords = Battleship.make_new_char_list rows (snd coordOne) (fst coordOne) 
        (fst coordTwo) [] in 
    {ship_list=init_ships; current_grid = Battleship.make_grid ship coords 
                               state.current_grid [];
     sunk_list=[]; ships_on_grid=ship::state.ships_on_grid}
  else if (fst coordOne = fst coordTwo && Pervasives.abs 
             (snd coordOne - snd coordTwo) = ship.size - 1)
  then
    let coords = Battleship.make_new_int_list (fst coordOne) columns (snd coordOne) 
        (snd coordTwo) [] in 
    {ship_list=init_ships; current_grid = Battleship.make_grid ship coords 
                               state.current_grid [];
     sunk_list=[]; ships_on_grid=ship::state.ships_on_grid}
  else raise OutOfBounds

(** [new_ship_list ship ship_list outlist] is the list of ships resulting from 
    finding [ship] in [ship_list] and incrementing its hit count.  *)
let rec new_ship_list ship ship_list outlist : ship list= 
  match ship_list with 
  | [] -> outlist
  | {name=nm;size=sz;hits=hts}::t when nm=ship.name -> 
    new_ship_list ship t ({name=ship.name;
                           size=ship.size; hits=hts+1}::outlist)
  | h::t -> new_ship_list ship t (h::outlist)

(** [sink_ship ship currentGrid outlist] is the grid that has located [ship] 
    and changed every point representing [ship] to the Sunk status. *)
let rec sink_ship ship (currentGrid:Battleship.grid) outlist = 
  match currentGrid with 
  |[] -> outlist 
  |((r,c),Hit({name=nm;size=sz;hits=hts}))::t when ship.name = nm -> 
    sink_ship ship t (((r,c),Sunk(ship))::outlist) 
  |((r,c),s)::t -> sink_ship ship t (((r,c),s)::outlist)

let rec hit_ship ship (currentGrid:Battleship.grid) r' c' outlist : Battleship.grid = 
  match currentGrid with 
  |[] -> outlist 
  |((r,c),Occupied({name=nm;size=sz;hits=hts}))::t when (ship.name = nm && (r'<>r || c'<>c)) -> print_endline "occupied branch 96";
    hit_ship ship t r' c' (((r,c),Occupied({ship with hits=hts+1}))::outlist)
  |((r,c),Occupied({name=nm;size=sz;hits=hts}))::t when (ship.name = nm && r'=r && c'=c) -> print_endline "occupied branch 98";
    hit_ship ship t r' c' (((r,c),Hit({ship with hits=hts+1}))::outlist)
  |((r,c),Hit({name=nm;size=sz;hits=hts}))::t when ship.name = nm -> print_endline "hit branch 100";
    hit_ship ship t r' c' (((r,c),Hit({ship with hits=ship.hits+1}))::outlist)
  |((r,c),s)::t -> hit_ship ship t r' c' (((r,c),s)::outlist)

let update_grid_occupied ship coord state (currentGrid:Battleship.grid) outlist = 
  let new_grid = (hit_ship ship state.current_grid (fst coord) (snd coord) []) in
  if ship.hits+1 = ship.size then
    sink_ship ship new_grid []
  else new_grid

(** [upgrade_grid_empty coord currentGrid outlist] is [currentGrid]
    with the point at the given coordinate changed from Empty status to Miss status. *)
let rec update_grid_empty coord (currentGrid:Battleship.grid) outlist =
  match currentGrid with
  | [] -> outlist
  | ((r,c),s)::t  -> if (r,c) = coord then (((r,c),Miss)::outlist) @ t
    else update_grid_empty coord t (((r,c),s)::outlist)

(** [is_sunk ship] is whether or not [ship] is of Sunk status. *)
let is_sunk ship : bool =
  if ship.hits>=ship.size then  true else false

(** [curr_sunk_list currShipList outlist] is the list of ships that have sunk in
    the current game.*)
let rec curr_sunk_list currShipList outlist = 
  match currShipList with 
  | [] -> outlist 
  | h::t -> if (is_sunk h) then curr_sunk_list t (h::outlist) 
    else curr_sunk_list t outlist

(** [fire coord currentState] is the new game state resulting from firing at 
     the given coordinate. *)
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
    | ((r,c),Hit(s))::t when (r,c)=coord -> 
      {ship_list = currShipList; current_grid = currGrid; 
       sunk_list = curr_sunk_list currShipList [];
       ships_on_grid = currentState.ships_on_grid}
    | ((r,c),Miss)::t when (r,c)=coord -> 
      {ship_list = currShipList; current_grid = currGrid; 
       sunk_list = curr_sunk_list currShipList [];
       ships_on_grid = currentState.ships_on_grid}
    | ((r,c),Occupied(s))::t when (r,c)=coord -> 
      let update_ship_list = new_ship_list s currShipList [] in 
      let update_grid_var = update_grid_occupied s coord currentState currGrid [] in 
      {ship_list=update_ship_list; current_grid=update_grid_var;
       sunk_list = curr_sunk_list update_ship_list [];
       ships_on_grid = currentState.ships_on_grid}
    | ((r,c),point)::t -> let new_state = fireHelper coord t currShipList in 
      if List.length new_state.current_grid < 100 then
        let new_grid = ((r,c),point)::new_state.current_grid in 
        { new_state with current_grid = new_grid}
      else new_state
  in fireHelper coord currentState.current_grid currentState.ship_list 

(** [placing] is whether or not the current game is still in placing mode. 
    i.e. have all the ships been placed? *)
let placing currentState : bool = 
  if List.length currentState.ships_on_grid <> 5 then true else false
(** [string_of_ships] is a string of the names of each type of ship. *)
let string_of_ships ship = 
  match ship.name with 
  | Carrier -> "Carrier"
  | Battleship -> "Battleship"
  | Cruiser -> "Cruiser"
  | Submarine -> "Submarine"
  | Destroyer -> "Destroyer"

(** [queue_helper currentState initships outlist] is the list of ships that still
    need to be placed. *)
let rec queue_helper currentState initships outlist : ship list = 
  let curr_ships_on_grid = currentState.ships_on_grid in 
  match initships with 
  | [] -> outlist 
  | h::t -> if List.mem h curr_ships_on_grid then queue_helper currentState t outlist 
    else queue_helper currentState t (h::outlist)

(** [queue currentState] is a well formatted, pretty string of the list of ships 
    that still need to be placed. *)
let queue currentState = 
  let ships_left = queue_helper currentState init_ships [] in 
  let ship_name ship = string_of_ships ship in 
  let concat a b = 
    if a="" then b else (a ^ ", " ^ b) in
  let ships_left_names = List.map ship_name ships_left in
  List.fold_left concat "" ships_left_names

(** [getAmountSunk] is the number of ships that have sunk in the current game. *)
let rec getAmountSunk lst accum = 
  match lst with
  | [] -> accum
  | h::t -> getAmountSunk t (accum + 1) 

let generate_0_1 () = 
  Random.int 2
let generate_0_3 () = 
  Random.int 4

let generate_rnd_row () = 
  List.nth (Battleship.rows) (Random.int 10) 

let generate_rnd_col () = 
  List.nth (Battleship.columns) (Random.int 10)

let int_choice elt1 elt2 = 
  if (elt1 > 11 || elt1 < 1) && not (elt2 > 11 || elt2 < 1) then elt2 
  else if (elt2 > 11 || elt2 < 1) && not (elt1 > 11 || elt1 < 1) then elt1 
  else if generate_0_1 () = 1 then elt1 
  else elt2

let char_choice  elt1 elt2 = 
  if ('a' <= elt1 && elt1 <= 'j') && not ('a' <= elt2 && elt2 <= 'j') then elt1 
  else if ('a' <= elt2 && elt2 <= 'j') && not ('a' <= elt1 && elt1 <= 'j') then elt2 
  else if generate_0_1 () = 1 then elt1 
  else elt2


let make_AI_coords decider (row: char) col rowcode (ship:Battleship.ship) = 
  if decider = 0 then 
    ( (row,col), (row,  (int_choice (col - ship.size + 1) (col + ship.size - 1))))  
  else 
    ( (row,col), (char_choice (Char.chr (rowcode - ship.size + 1 )) (Char.chr (rowcode + ship.size - 1)),col)  )


let sort_tuple tup = 
  match tup with 
  |((r1, c1), (r2, c2)) -> if c2 > c1 || r2 > r1 then ((r1, c1), (r2, c2)) 
    else ((r2, c2), (r1, c1))


let output_AI_coords ship = 
  let rnd_0_1 = generate_0_1 () in
  let c = generate_rnd_col () in
  let r = generate_rnd_row () in 
  let r_code = Char.code r in 
  let unsorted_tuple = make_AI_coords rnd_0_1 r c r_code ship in 
  sort_tuple unsorted_tuple

let rec state_builder_AI (currState:state) (ships:ship list) =
  match ships with
  | [] -> currState
  | ship::t -> 
    try 
      let coords = (output_AI_coords ship) in 
      let new_state = (place ship (fst coords) (snd coords) currState) in
      if List.length new_state.ships_on_grid = List.length currState.ships_on_grid
      then state_builder_AI currState ships
      else state_builder_AI new_state t
    with 
    | _ -> state_builder_AI currState ships

let can_fire (point:Battleship.point) = 
  match point with 
  | ((r,c), Hit(s)) -> false
  |((r,c), Sunk(s)) -> false 
  |((r,c), Miss) -> false 
  |_ -> true 

let rec get_point (coord:Battleship.coordinate) (grid: Battleship.grid) (fullgrid:Battleship.grid)= 
  match grid with 
  |[] -> failwith"coord does not exist in grid"
  |h::t -> if fst(h) = coord then h else get_point coord t fullgrid

let pick_adjacent grid (point:Battleship.point) (rowcode: int) s : Battleship.coordinate = 
  if 'a' <= (Char.chr (rowcode - 1)) && (Char.chr (rowcode - 1)) <= 'j' && 1 <= (snd (fst point)) && (snd (fst point))<= 10 &&
     can_fire (get_point (Char.chr (rowcode - 1) , snd (fst point)) grid grid) then (Char.chr (rowcode - 1) , snd (fst point))
  else if 'a' <= (Char.chr (rowcode + 1)) && (Char.chr (rowcode + 1)) <= 'j' && 1 <= (snd (fst point)) && (snd (fst point))<= 10   && 
          can_fire (get_point (Char.chr (rowcode + 1) , snd (fst point)) grid grid) then (Char.chr (rowcode + 1) , snd (fst point)) 
  else if 'a' <= (fst(fst point)) && (fst(fst point)) <= 'j' && 1 <= (snd (fst point)-1) && (snd (fst point)-1)<= 10  && 
          can_fire (get_point ((fst(fst point) , snd(fst point)-1)) grid grid) then (fst(fst point) , snd(fst point)-1)
  else if 'a' <= (fst(fst point)) && (fst(fst point)) <= 'j' && 1 <= (snd (fst point)+1) && (snd (fst point)+1)<= 10  && 
          can_fire (get_point ((fst(fst point) , snd(fst point)+1)) grid grid) then (fst(fst point) , snd(fst point)+1)
  else (generate_rnd_row (), generate_rnd_col ())


let rec fire_AI_coords (fullgrid: Battleship.grid) (grid:Battleship.grid) : coordinate = 
  match grid with 
  |[] -> (generate_rnd_row (), generate_rnd_col ())
  |((r,c), Hit(s))::t -> pick_adjacent fullgrid ((r,c), Hit(s)) (Char.code r) s
  |h::t -> fire_AI_coords fullgrid t 

(** [winOrNot] is whether or not all ships have sunk in this game. *)
let winOrNot lst : bool = 
  if List.length lst = 5 then true else false
