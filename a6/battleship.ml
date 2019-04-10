
type name = Carrier | Battleship | Cruiser | Submarine | Destroyer 
type ship = {name: name; size: int; hits: int}
type coordinate = char * int 
type status = Occupied of ship | Hit | Empty  
type point = coordinate * status
type grid = point list

let carrier = {name = Carrier; size = 5; hits = 0}
let battleship = {name = Battleship; size = 4; hits = 0}
let cruiser = {name = Cruiser; size = 3; hits = 0}
let submarine = {name = Submarine; size = 3; hits = 0}
let destroyer = {name = Destroyer; size = 2; hits = 0}

let rows = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j']
let columns = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let rec pair (r: char) (cols: int list) outlist = 
  match cols with 
  |[] -> outlist 
  |h::t -> pair r t (((r,h),Empty)::outlist)

let rec init_grid (r: char list) (c: int list) (outlist: grid) = 
  match r with 
  |[] -> outlist
  |h::t -> init_grid t c ((pair h c [])@ outlist)

let rec make_new_int_list (r:char) (c:int list) (c1:int) (c2:int) outlist = 
  match c with 
  |[] -> outlist
  |h::t when h>=c1 && h<=c2 -> make_new_int_list r t c1 c2 ((r,h)::outlist)
  |h::t -> outlist

let rec make_new_char_list (r:char list) (c:int) (r1:char) (r2:char) outlist = 
  match r with 
  |[] -> outlist
  |h::t when h>=r1 && h<=r2 -> make_new_char_list t c r1 r2 ((h,c)::outlist)
  |h::t -> outlist

let rec make_grid ship ship_coords grid (outlist: grid) = 
  match grid with 
  | [] -> outlist
  | ((r,c),state)::t when state=Empty -> if (List.mem (r,c) ship_coords) 
    then make_grid ship ship_coords t ((r,c),Occupied(ship))::outlist
  | _ -> raise(Failure "A ship is already placed here")



let place (ship:ship) (coordOne:coordinate) (coordTwo:coordinate) (grid:grid) = 
  if not ((fst coordOne = fst coordTwo) || (snd coordOne = snd coordTwo)
          || (fst coordOne = fst coordTwo && snd coordOne = snd coordTwo)) 
  then raise (Failure "Invalid Coords")
  else if (snd coordOne = snd coordTwo && Pervasives.abs (Char.code (fst coordOne) 
                                                          - Char.code(fst coordTwo)) = ship.size ) 
  then 
    let coords = make_new_char_list rows (snd coordOne) (fst coordOne) (fst coordTwo) [] in 

  else if (fst coordOne = fst coordTwo && Pervasives.abs (snd coordOne - snd coordTwo) = ship.size)
  then 
    let coords = make_new_int_list (fst coordOne) columns (snd coordOne) (snd coordTwo) [] in 












