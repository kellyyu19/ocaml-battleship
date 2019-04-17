(** The type name that is one-of these. *)
type name = Carrier | Battleship | Cruiser | Submarine | Destroyer 

(** The type ship that contains the name, size, and hits. *)
type ship = {name: name; size: int; hits: int}

(** The type coordinate that is a tuple of a char and an int. *)
type coordinate = char * int 

(** The type status that is one-of these. *)
type status = Occupied of ship | Hit of ship | Sunk of ship | Empty | Miss

(** The type point that is a tuple of a coordinate and a status. *)
type point = coordinate * status

(** The type grid that is a point list *)
type grid = point list

(** Raised when a ship is already placed at given coordinates. *)
exception ShipHere

(** Raised when there is an invalid command. *)
exception Malformed 

let rows = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j']
let columns = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

(** [pair r c outlist] is a list of points that is formed with a tuple of 
    a tuple of [r] and each element of [cols] and the status Empty.
    Requires: [cols] is not empty. *)
let rec pair (r: char) (cols: int list) outlist = 
  match cols with 
  |[] -> outlist 
  |h::t -> pair r t (((r,h),Empty)::outlist)

(** [init_grid r c outlist] is the initial grid that is formed, with all
    points having the status Empty. 
    Requires: [r] is not empty. 
            [c] is not empty.  *)
let rec init_grid (r: char list) (c: int list) (outlist: grid) = 
  match r with 
  |[] -> outlist
  |h::t -> init_grid t c ((pair h c [])@ outlist)

(** [make_new_int_list r c c1 c2 outlist] is a list of coordinates, where each
    coordinate is a tuple of [r] and an integer between [c1] and [c2], inclusive. 
    Requires: [c] is not empty. *)
let rec make_new_int_list (r:char) (c:int list) (c1:int) (c2:int) outlist = 
  match c with 
  |[] -> outlist
  |h::t when h>=c1 && h<=c2 -> make_new_int_list r t c1 c2 ((r,h)::outlist)
  |h::t -> make_new_int_list r t c1 c2 outlist

(** [make_new_char_list r c c1 c2 outlist] is a list of coordinates, where each
    coordinate is a tuple of a char between [r1] and [r2],inclusive, and an 
    integer [c].
    Requires: [r] is not empty. *)
let rec make_new_char_list (r:char list) (c:int) (r1:char) (r2:char) outlist = 
  match r with 
  |[] -> outlist
  |h::t when h>=r1 && h<=r2 -> make_new_char_list t c r1 r2 ((h,c)::outlist)
  |h::t -> make_new_char_list t c r1 r2 outlist

(** [make_grid ship ship_coords grid outlist] is a grid that is formed 
    depending on the status of each point. 
    Requires: [ship_coords] is not empty.
              [grid] is not empty. 
    Raises: [ShipHere] if there is already occupied at a point where a new
            ship is trying to be placed
            [Malformed] when there is an invalid command. *)
let rec make_grid ship ship_coords grid (outlist: grid) : grid = 
  match grid with 
  | [] -> outlist
  | ((r,c),state)::t when state=Empty -> if (List.mem (r,c) ship_coords) 
    then make_grid ship ship_coords t (((r,c),Occupied(ship))::outlist)
    else make_grid ship ship_coords t (((r,c),Empty)::outlist)
  | ((r,c),Occupied(s))::t -> if (List.mem (r,c) ship_coords)
    then raise ShipHere 
    else make_grid ship ship_coords t (((r,c),Occupied(s))::outlist)
  | _ -> raise Malformed














