type coordinate = char * int 
type status = Occupied | Hit | Empty  
type point = coordinate * status
type grid = point list 

type name = Carrier | Battleship | Cruiser | Submarine | Destroyer  
type ship = {name: name; size: int }


let carrier = {name = Carrier; size = 5}
let battleship = {name = Battleship; size = 4}
let cruiser = {name = Cruiser; size = 3}
let submarine = {name = Submarine; size = 3}
let destroyer = {name = Destroyer; size = 2}



