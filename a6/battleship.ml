
type name = Carrier | Battleship | Cruiser | Submarine | Destroyer 
type ship = {name: name; size: int; hits: int}
type coordinate = char * int 
type status = Occupied of ship | Hit of ship | Sunk of ship | Empty | Miss
type point = coordinate * status
type grid = point list

exception ShipHere
exception Malformed 


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
  |h::t -> make_new_int_list r t c1 c2 outlist

let rec make_new_char_list (r:char list) (c:int) (r1:char) (r2:char) outlist = 
  match r with 
  |[] -> outlist
  |h::t when h>=r1 && h<=r2 -> make_new_char_list t c r1 r2 ((h,c)::outlist)
  |h::t -> make_new_char_list t c r1 r2 outlist

let rec make_grid ship ship_coords grid (outlist: grid) : grid = 
  match grid with 
  | [] -> outlist
  | ((r,c),state)::t when state=Empty -> if (List.mem (r,c) ship_coords) 
    then make_grid ship ship_coords t (((r,c),Occupied(ship))::outlist)
    else make_grid ship ship_coords t (((r,c),Empty)::outlist)
  | ((r,c),Occupied(s))::t -> if (List.mem (r,c) ship_coords)
    then raise ShipHere else make_grid ship ship_coords t (((r,c),Occupied(s))::outlist)
  | _ -> raise Malformed














