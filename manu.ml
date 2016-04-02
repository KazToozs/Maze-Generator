type cell = {idx : int}
type door = {is_open: bool}
let x = ref 0
let y = ref 0
let listCell = ref []
let listDoorH = ref []
let listDoorV = ref []

let rec fill_cell a b =
  match b with
  | 0 -> []
  | _ ->
     (match a with
      | 0 -> fill_cell !x (b - 1);
      | _ -> {idx = a - 1 + (b - 1) * !x}::fill_cell (a - 1) b)

let rec fill_doorH a b =
  match b with
  | 0 -> []
  | 1 -> []
  | _ -> 
     (match a with
      | 0 -> fill_doorH !x (b - 1)
      | _ -> {is_open = false}::fill_doorH (a - 1) b)

let rec fill_doorV a b =
  match b with
  | 0 -> []
  | _ ->
     (match a with
      | 0 -> fill_doorV !x (b - 1)
      | 1 -> fill_doorV !x (b - 1)
      | _ -> {is_open = true}::fill_doorV (a - 1) b)

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
          print_string "|   ";
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
  if ((x + (y * (x_lim - 1))) < (x_lim * y_lim)) && y < y_lim
  then
    begin
      if count == 0
      then
        begin
          print_string "\n+";
          print_Vdoor listDoorV (count + 1) x x_lim y y_lim
          end
      else if count < (x_lim + 1)
      then
        begin
          if (door_is_open listDoorV x y x_lim) == true
          then
            begin
                print_string "   +";
                if count == x_lim
                then
                  print_char '\n';
                print_Vdoor listDoorV (count + 1) x x_lim y y_lim
            end
          else
            begin
              print_string "---+";
              if count == x_lim
              then
                print_char '\n';
              print_Vdoor listDoorV (count + 1) x x_lim y y_lim
            end
        end
    end

(* let print_first_line count = *)
(*   if count == 0 *)
(*   then *)
(*     print_string "+--+"; *)
(*   else if count != 0 *)
(*   then *)
(*     print_string "--+"; *)

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
               print_string "+\n";
               print_doorH listDoorH 0 x x_lim y y_lim;
               print_maze body listDoorH listDoorV x_lim y_lim 0 (y + 1)
             end
         end
       else
         begin
           if y != y_lim + 1
           then
             print_Vdoor listDoorV 0 x x_lim y y_lim;
           if y < y_lim
           then
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
  print_maze !listCell !listDoorH !listDoorV !x !y 0 0;
;;
  
let main =
  maze
;;
