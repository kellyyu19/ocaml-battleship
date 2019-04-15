open Battleship

let rec point_compare (point1:Battleship.point) (point2:Battleship.point) = 
  match point1, point2 with 
  |((r1,c1),s1), ((r2,c2),s2) -> if c1 > c2 then 1 
    else if c1 < c2 then -1 
    else 0

let sort_row (row: Battleship.point list)  = 
  List.sort (point_compare) row

let rec group_row char (grid:Battleship.grid) outlist = 
  match grid with 
  |[] -> outlist 
  |((r,c),s)::t -> if r = char then group_row char t (((r,c),s)::(outlist)) else 
      group_row char t outlist

let rec sort_and_group_rows rows (grid:Battleship.grid) outlist = 
  match rows with 
  |[] -> outlist 
  |h::t -> sort_and_group_rows t grid ((sort_row(group_row h grid []))::outlist)


let rec print_row sortedrow outstring = 
  match sortedrow with 
  |[]-> outstring
  |((r,c),Hit(s))::t -> print_row t (outstring ^ "*     ") 
  |((r,c),Sunk(s))::t -> print_row t (outstring ^ "/     ") 
  |((r,c),Empty)::t -> print_row t (outstring ^ "-     ") 
  |((r,c),Occupied(s))::t -> print_row t (outstring ^ "-     ")
  |((r,c),Miss)::t -> print_row t (outstring ^ "x     ")


let labeled_row sortedrow = 
  match sortedrow with 
  |[]-> ""
  |((r,_),_)::t -> ((Char.escaped r) ^"       "^ (print_row sortedrow ""))


let rec text_grid (rowlist:Battleship.point list list) outstring = 
  match rowlist with
  |[]->  (
      "        1     2     3     4     5     6     7     8     9     10" 
      ^"\n"^ outstring)
  |h::t -> text_grid t (outstring ^"\n"^"\n"^ labeled_row h)


