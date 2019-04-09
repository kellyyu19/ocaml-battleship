
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

let place (ship:ship) (coordOne:coordinate) (coordTwo:coordinate) (grid:grid) : grid = 
  if not ((fst coordOne = fst coordTwo) || (snd coordOne = snd coordTwo)) 
  then raise (Failure "Invalid Coords")
  else if (fst coordOne = fst coordTwo) then 










