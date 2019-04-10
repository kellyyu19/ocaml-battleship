open Battleship

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

let rec make_grid ship ship_coords grid (outlist: Battleship.grid) : grid = 
  match grid with 
  | [] -> outlist
  | ((r,c),state)::t when state=Empty -> if (List.mem (r,c) ship_coords) 
    then make_grid ship ship_coords t (((r,c),Occupied(ship))::outlist)
    else raise(Failure "This coordinate is not in the grid")
  | _ -> raise(Failure "A ship is already placed here")


let place (ship:ship) (coordOne:coordinate) (coordTwo:coordinate) (grid:grid) = 
  if not ((fst coordOne = fst coordTwo) || (snd coordOne = snd coordTwo)
          || (fst coordOne = fst coordTwo && snd coordOne = snd coordTwo)) 
  then raise (Failure "Invalid Coords")
  else if (snd coordOne = snd coordTwo && Pervasives.abs (Char.code (fst coordOne) 
                                                          - Char.code(fst coordTwo)) = ship.size ) 
  then 
    let coords = make_new_char_list rows (snd coordOne) (fst coordOne) (fst coordTwo) [] in 
    make_grid ship coords grid []
  else if (fst coordOne = fst coordTwo && Pervasives.abs (snd coordOne - snd coordTwo) = ship.size)
  then 
    let coords = make_new_int_list (fst coordOne) columns (snd coordOne) (snd coordTwo) [] in 
    make_grid ship coords grid []
  else raise (Failure "Invalid Coords")