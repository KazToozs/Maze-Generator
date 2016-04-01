type cell = {posx : int; posy : int; idx : int}
type door = {is_open: bool; first_door_idx : int; second_door_idx : int}

let fill_cell x y =
  let rec fill_cell_b a b =
    if b != 0
    then if a != 0
         then {posx = a - 1; posy = b - 1; idx = a - 1 + (b - 1) * x}::fill_cell_b (a - 1) b
         else fill_cell_b x (b - 1)
    else []
  in (fill_cell_b x y);;


let fill_doorV x y =
  let rec fill_door_a a b =
    if b > 1
    then if a != 0
         then {is_open = false; first_door_idx = ((x * (b - 1)) + a - x - 1);
               second_door_idx = ((x * (b - 1)) + a - 1)}::fill_door_a (a - 1) b
         else fill_door_a x (b - 1)
    else []
  in (fill_door_a x y);;

let fill_doorH x y =
  let rec fill_door_b a b =
    if b != 0
    then if a > 1
         then {is_open = false; first_door_idx = (a - 1 + ((b - 1) * x - 1));
               second_door_idx = (a - 1 + ((b - 1) * x))}::fill_door_b (a - 1) b
         else fill_door_b x (b - 1)
    else []
  in (fill_door_b x y);;
