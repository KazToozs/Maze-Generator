let count = ref 0
let x = ref 4
let y = ref 3
type cell = {posx : int; posy : int; idx : int}
type door = {is_open: bool; first_door_idx : int; second_door_idx : int}
let listCell = ref []
let listDoor = ref []
                   
let rec fill_cell a b =
  if b != 0
  then if a != 0
       then {posx = a - 1; posy = b - 1; idx = a - 1 + (b - 1) * !x}::fill_cell (a - 1) b
       else fill_cell !x (b - 1)
  else []

let rec fill_door_a a b =
  if b > 1
  then if a != 0
       then {is_open = false; first_door_idx = ((!x * (b - 1)) + a - !x - 1);
             second_door_idx = ((!x * (b - 1)) + a - 1)}::fill_door_a (a - 1) b
       else fill_door_a !x (b - 1)
  else []

let rec fill_door_b a b =
  if b != 0
  then if a > 1
       then {is_open = false; first_door_idx = (a - 1 + ((b - 1) * !x - 1));
             second_door_idx = (a - 1 + ((b - 1) * !x))}::fill_door_b (a - 1) b
       else fill_door_b !x (b - 1)
  else []

let fill_door a b =
  (fill_door_a a b)@(fill_door_b a b)

let rec print_rest nb reference =
  if (nb != 0)
  then if nb == reference
       then
         begin
           print_char '\n';
           print_string "|   ";
           print_rest (nb - 1) reference;
         end
       else if nb != 1
       then
         begin
           print_string "    ";
           print_rest (nb - 1) reference;
         end
       else
         begin
           print_string "   |";
           print_rest (nb - 1) reference;
           end
  else
    print_char '\n'

let rec print_last_line nb reference =
  if (nb != 0)
  then
    begin
      print_string "+--+";
      print_last_line (nb - 1) reference;
    end
  else
    print_char '\n'

let rec print_maze listCell count nbTurn =
  match listCell with
  | [] -> []
  | head::rest ->
     begin
       print_string "+--+";
       if (count + 1) mod !x == 0 && count != 0
       then 
         begin
           print_rest (count + 1) !x;
           if ((nbTurn + 1) == !y && (count + 1) mod !x == 0 && count != 0)
           then print_last_line (count + 1) !x;
           print_maze rest 0 (nbTurn + 1);
         end
       else print_maze rest (count + 1) nbTurn;
     end

let main () =
  begin
    x := int_of_string(Sys.argv.(1));
    y := int_of_string(Sys.argv.(2));
    listCell := fill_cell !x !y;
    listDoor := fill_door !x !y;
  end
;;
