type cell = {posx : int; posy : int; idx : int}
type door = {is_open: bool; first_door_idx : int; second_door_idx : int}
let x = ref 0
let y = ref 0
let listCell = ref []
let listDoorH = ref []
let listDoorV = ref []

let rec fill_cell a b =
  if b != 0
  then if a != 0
       then
         {posx = a - 1; posy = b - 1; idx = a - 1 + (b - 1) * !x}::fill_cell (a - 1) b
       else fill_cell !x (b - 1)
  else []

let rec fill_doorH a b =
  if b > 1
  then if a != 0
       then {is_open = false; first_door_idx = ((!x * (b - 1)) + a - !x - 1);
             second_door_idx = ((!x * (b - 1)) + a - 1)}::fill_doorH (a - 1) b
       else fill_doorH !x (b - 1)
  else []

let rec fill_doorV a b =
  if b != 0
  then if a > 1
       then {is_open = false; first_door_idx = (a - 1 + ((b - 1) * !x - 1));
             second_door_idx = (a - 1 + ((b - 1) * !x))}::fill_doorV (a - 1) b
         else fill_doorV !x (b - 1)
  else []

let door_is_open listDoorH x y x_lim =
  (List.nth listDoorH (x + (y * (x_lim - 1)) - 1)).is_open;
;;

let rec print_doorH listDoorH count x x_lim y y_lim =
  if ((x + (y * (x_lim - 1)) - 1) < (x_lim * y_lim))
  then
    begin
      if count == 0
      then
        begin
          print_string "\n|   ";
          print_doorH listDoorH (count + 1) x x_lim y y_lim
        end
      else if count != (x_lim)
      then
        begin
          if (door_is_open listDoorH count y x_lim) == true
          then
            begin
              print_string "    ";
              print_doorH listDoorH (count + 1) x x_lim y y_lim
            end
          else
            begin
              print_string "|   ";
              print_doorH listDoorH (count + 1) x x_lim y y_lim
            end
        end
      else if count == (x_lim)
      then
        begin
          print_char '|';
        end
    end

let rec print_Vdoor listDoorV count x x_lim y y_lim=
  if ((x + (y * (x_lim - 1)) - 1) < (x_lim * y_lim))
  then
    begin
      begin
        if count == 0
        then
          begin
            print_string "\n+";
            print_Vdoor listDoorV (count + 1) x x_lim y y_lim
          end
        else if count != (x_lim + 1)
        then
          begin
            if (door_is_open listDoorV x y x_lim) == true
            then
              begin
                print_string "  +";
                print_Vdoor listDoorV (count + 1) x x_lim y y_lim
              end
            else
              begin
                print_string "---+";
                print_Vdoor listDoorV (count + 1) x x_lim y y_lim
              end
          end
      end
    end

let rec print_maze listCell listDoorH listDoorV x_lim y_lim x y =
  match listCell with
  | [] -> []
  | head::body ->
     begin
       if y == 0
       then
         begin
           if x != x_lim
           then
             begin
               print_string "+--+";
               print_maze body listDoorH listDoorV x_lim y_lim (x + 1) y
             end
           else
             begin
               print_char '+';
               print_doorH listDoorH 0 x x_lim y y_lim;
               print_maze body listDoorH listDoorV x_lim y_lim 0 (y + 1)
             end
         end
       else if x == x_lim
       then
         begin
           print_doorH listDoorH 0 x x_lim y y_lim;
           print_maze body listDoorH listDoorV x_lim y_lim (x + 1) y
         end
       (* else if y != 0 && x != x_lim *)
       (* then *)
       (*   begin *)
       (*     print_doorH listDoorH 0 x x_lim y; *)
       (*     print_maze body listDoorH listDoorV x_lim y_lim (x + 1) y *)
       (*   end *)
       else
         begin
           print_Vdoor listDoorV 0 x x_lim y y_lim;
           print_doorH listDoorH 0 x x_lim y y_lim;
           print_maze body listDoorH listDoorV x_lim y_lim 0 (y + 1)
         end
     end

let maze =
  x := int_of_string(Sys.argv.(1));
  y := int_of_string(Sys.argv.(2));
  listCell := fill_cell !x !y;
  listDoorH := fill_doorH !x !y;
  listDoorV := fill_doorV !x !y;
  print_maze !listCell !listDoorH !listDoorV !x !y 0 0
;;
  
let main =
  maze
;;
