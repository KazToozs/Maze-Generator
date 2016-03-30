let x = ref 3
let y = ref 3
type cell = {posx : int; posy : int; idx : int}
type door = {is_open: bool; first_door_idx : int; second_door_idx : int}
let listCell = ref []
let listDoor = ref []
                   
let rec fill_cell a b =
  if (b != 0)
  then (if (a != 0)
        then {posx = a - 1; posy = b - 1; idx = a - 1 + (b - 1) * !x}::fill_cell (a - 1) b
        else fill_cell !x (b - 1))
  else []

let rec fill_door a b =
  if (b > 1)
  then (if (a != 0)
        then {is_open = false; first_door_idx = ((!x * (b - 1)) + a - !x - 1);
              second_door_idx = ((!x * (b - 1)) + a - 1)}::fill_door(a - 1) b
        else fill_door !x (b - 1))
  else []

let main () =
  begin
    x := int_of_string(Sys.argv.(1));
    y := int_of_string(Sys.argv.(2));
    listCell := fill_cell !x !y;
    listDoor := fill_door !x !y;
  end
;;
