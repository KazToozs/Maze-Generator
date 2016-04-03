type path = {prevMove : int}
type cell = {idx : int; visited : bool}
type door = {isOpen: bool; fstCellIdx : int; scdCellIdx : int}
let rand = ref 0
let cell_end = ref 0
let start = ref 0
let x = ref 0
let y = ref 0
let listPath = ref []
let listCell = ref []
let listDoorH = ref []
let listDoorV = ref []

let rec fill_cell a b l = match b with
  | 1 -> (match a with
          | 0 -> l;
          | _ -> fill_cell (a - 1) b ({idx = a - 1; visited = false}::l)
         )
  | _ -> (match a with
          | 0 -> fill_cell !x (b - 1) (l);
          | _ -> fill_cell (a - 1) b ({idx = (a - 1) + (b - 1) * !x; visited = false}::l)
         )

let rec fill_doorV a b l = match b with
  | 0 -> l;
  | _ -> (match a with
          | 0 -> fill_doorV !x (b - 1) (l);
          | a when a = !x -> fill_doorV (a - 1) b l;
          | _ -> fill_doorV (a - 1)
                            b
                            ({isOpen = false; fstCellIdx = ((!x * (b - 1) + a - 1));
                              scdCellIdx = ((!x * (b - 1) + a))}::l)
         )

let rec fill_doorH a b l = match b with
  | 0 -> l;
  | b when b = !y -> fill_doorH a (b - 1) l;
  | _ -> (match a with
          | 0 -> fill_doorH !x (b - 1)
                            ({isOpen = false; fstCellIdx = (a + !x * (b - 1));
                              scdCellIdx = (a + !x * b)}::l);
          | a when a = !x -> fill_doorH (a - 1) b (l);
          | _ -> fill_doorH (a - 1) b 
                            ({isOpen = false; fstCellIdx = (a + !x * (b - 1));
                              scdCellIdx = (a + !x * b)}::l);
         )



(* let rec fill_cell a b = *)
(*   match b with *)
(*   | 0 -> [] *)
(*   | _ -> *)
(*      (match a with *)
(*       | 0 -> fill_cell !x (b - 1); *)
(*       | _ -> {idx = a - 1 + (b - 1) * !x; visited = false}::fill_cell (a - 1) b) *)

(* let rec fill_doorV a b = *)
(*   match b with *)
(*   | 0 -> [] *)
(*   | 1 -> [] *)
(*   | _ -> *)
(*      (match a with *)
(*       | 0 -> fill_doorV !x (b - 1) *)
(*       | _ -> {isOpen = false; fstCellIdx = ((!x * (b - 1) + a - !x - 1)); *)
(*              scdCellIdx = ((!x * (b - 1) + a - 1))}::fill_doorV (a - 1) b) *)
      
(* let rec fill_doorH a b = *)
(*   match b with *)
(*   | 0 -> [] *)
(*   | _ -> *)
(*      (match a with *)
(*       | 1 -> fill_doorH !x (b - 1) *)
(*       | _ -> {isOpen = false; fstCellIdx = ((!x * (b - 1)) + a - 2); *)
(*              scdCellIdx = ((!x * (b - 1)) + a - 1)}::fill_doorH (a - 1) b) *)

let doorIsOpen listDoorH x y x_lim =
  (List.nth listDoorH (x + (y * (x_lim - 1)) - 1)).isOpen;
;;

let doorIsOpenV listDoorV x y x_lim =
  (List.nth listDoorV (x + ((y - 1) * (x_lim)))).isOpen;
;;

let rec print_doorH listDoorH count x x_lim y y_lim =
  if ((x + (y * (x_lim - 1)) - 1) < (x_lim * y_lim))
  then
    begin
      if count = 0
      then
        begin
          print_string "|   ";
          print_doorH listDoorH (count + 1) x x_lim y y_lim
        end
      else if count != (x_lim)
      then
        begin
          if (doorIsOpen listDoorH count y x_lim) == true
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
      else if count = (x_lim)
      then
        begin
          print_char '|';
        end
    end

let rec print_Vdoor listDoorV count x x_lim y y_lim=
  if ((x + (y * (x_lim - 1))) < (x_lim * y_lim)) && y < y_lim
  then
    begin
      if count = 0
      then
        begin
          print_string "\n+";
          print_Vdoor listDoorV (count + 1) x x_lim y y_lim
          end
      else if count < (x_lim + 1)
      then
        begin
          if (doorIsOpenV listDoorV (count - 1) y x_lim) == true
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

let rec print_last_line count x_lim =
  if count != 0
  then
    begin

      if count = 1
      then
        begin
          print_string "---+\n";
          print_last_line (count - 1) x_lim;
        end
      else if count == x_lim
      then
        begin
          print_string "\n+---+";
          print_last_line (count - 1) x_lim;
        end
      else
        begin
          print_string "---+";
          print_last_line (count - 1) x_lim;
        end
    end

let rec print_maze listCell listDoorH listDoorV x_lim y_lim x y =
  match listCell with
  | [] -> print_last_line x_lim x_lim
  | head::body ->
     begin
       if y = 0
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

let getRightCell listDoorH pos listCell =
  if ((pos + 1) mod !x != 0)
  then
    begin
      match (List.nth listCell (pos + 1)).visited with
      | false ->
         true
      | true -> false
    end
  else
    false;
;;

let getLeftCell listDoorH pos listCell =
  if (pos mod !x != 0)
  then
    begin
      match (List.nth listCell (pos - 1)).visited with
      | false ->
       true
      | true -> false
    end
  else
    false;
;;

let getUpCell listDoorH pos listCell =
  if pos >= !x
  then
    begin
      match (List.nth listCell (pos - !x)).visited with
      | false ->
         true
      | true ->
         false
    end
  else
    false;
;;

let getDownCell listDoorH pos listCell =
  if pos <= ((!x * !y) - 1 - !x)
  then
    begin
      match (List.nth listCell (pos + !x)).visited with
      | false ->
         true
      | true -> false
    end
  else
    false;
;;

(* let rec putVisited listCell toMatch = *)
(*   match listCell with *)
(*   | head::body ->      *)
(*      if (toMatch.idx = head.idx) *)
(*      then {idx = head.idx; visited = true}::(putVisited body toMatch) *)
(*      else head::(putVisited body toMatch) *)
(*   | [] -> [] *)
(* ;; *)

let rec putVisited listCell toMatch l =
  match listCell with
  | head::body ->
     if (toMatch.idx = head.idx)
     then putVisited body toMatch ({idx = head.idx; visited = true}::body)
     else putVisited body toMatch (head::body)
  | [] -> l
;;
  
let rec putOpen listDoor toMatch l =
  match listDoor with
  | head::body ->
     if (toMatch.fstCellIdx = head.fstCellIdx && toMatch.scdCellIdx = head.scdCellIdx)
     then putOpen body toMatch ({isOpen = true; fstCellIdx = head.fstCellIdx; scdCellIdx = head.scdCellIdx}::body)
     else putOpen body toMatch (head::body)
  | [] -> l
;;

let open_door pos move =
  if move = 1
  then
    begin
      listCell := putVisited !listCell (List.nth !listCell (pos + 1)) [];
      listDoorH := putOpen !listDoorH (List.nth !listDoorH (pos - (pos / !y))) [];
    end
  else if move = 2
  then
    begin
      listCell := putVisited !listCell (List.nth !listCell (pos - 1)) [];
      listDoorH := putOpen !listDoorH (List.nth !listDoorH (pos - (pos / !y) - 1)) [];
    end
  else if move = 3
  then
    begin
      listCell := putVisited !listCell (List.nth !listCell (pos - !x)) [];
      listDoorV := putOpen !listDoorV (List.nth !listDoorV (pos - !x)) [];
    end
  else if move = 4
  then
    begin
      listCell := putVisited !listCell (List.nth !listCell (pos + !x)) [];
      listDoorV := putOpen !listDoorV (List.nth !listDoorV pos) [];
    end
;;

let add_Path pos =
  List.append !listPath [{prevMove = pos}]
;;

let getPrevMove listPath =
  if (List.length listPath) > 1
  then
    (List.hd (List.rev listPath)).prevMove
  else
    !start
;;

let removeLastElem listPath =
  List.rev (List.tl (List.rev listPath))
;;

let check_Possibility pos =
  if (getRightCell !listDoorH pos !listCell) = false
     && (getLeftCell !listDoorH pos !listCell) = false
     && (getUpCell !listDoorV pos !listCell) = false
     && (getDownCell !listDoorV pos !listCell) = false
  then false
  else
    true
;;

let print_wich_possibility pos =
  if (getRightCell !listDoorH pos !listCell) = true
  then print_string "right\n"
  else if (getLeftCell !listDoorH pos !listCell) = true
  then print_string "left\n"
  else if (getUpCell !listDoorV pos !listCell) = true
  then print_string "Up\n"
  else if (getDownCell !listDoorV pos !listCell) = true
  then print_string "Down\n"
;;

let is_Finished pos =
  if (check_Possibility pos) = false
  then
    true
  else
    false;
;;

let rec fill_path pos =
  rand := (Random.int 4);
  if (check_Possibility pos) = true
  then
    begin
      if (is_Finished pos) = false
      then
        begin
          if !rand = 1
          then
            begin
              if (getRightCell !listDoorH pos !listCell) == true
              then
                begin
                  open_door pos 1;
                  listPath := add_Path (pos + 1);
                  fill_path (pos + 1);
                end
              else
                fill_path pos;
            end
          else if !rand = 2
          then
            begin
              if (getLeftCell !listDoorH pos !listCell) == true
              then
                begin
                  open_door pos 2;
                  listPath := add_Path (pos - 1);
                  fill_path (pos - 1);
                end
              else
                fill_path pos;
            end
          else if !rand = 3
          then
            begin
              if (getUpCell !listDoorV pos !listCell) == true
              then
                begin
                  open_door pos 3;
                  listPath := add_Path (pos - !x);
                  fill_path (pos - !x);
                end
              else
                fill_path pos;
            end
          else
            begin
              if (getDownCell !listDoorV pos !listCell) == true
              then
                begin
                  open_door pos 4;
                  listPath := add_Path (pos + !x);
                  fill_path (pos + !x);
                end
              else
                fill_path pos;
            end
        end
    end
  else if pos != !start
  then
    begin
      (* print_maze !listCell !listDoorH !listDoorV !x !y 0 0; *)
      rand := getPrevMove !listPath;
      listPath := removeLastElem !listPath;
      fill_path !rand;
    end
;;

let rec print_list listCell =
  match listCell with
  | [] -> [];
  | head::body ->
     (match head.visited with
      | true -> print_int 1;
                print_list body;
      | false ->
         print_int 0;
         print_list body)
;;

(* let check_Arg = *)
(*   if ((Array.length Sys.argv - 1) != 4 && (Array.length Sys.argv - 1) != 0) then *)
(*     begin *)
(*       print_endline "Usage: ./step1 -r [x > 1] -c [y > 1]"; *)
(*       false; *)
(*     end *)
(*   else *)
(*     begin *)
(*       if ((String.compare Sys.argv.(1) "-r") = 0)  *)
(*       then *)
(*         try int_of_string(Sys.argv.(2)) with *)
(*         |  *)
(*         x := int_of_string(Sys.argv.(2)); *)
(*       y := int_of_string(Sys.argv.(2)); *)
(* ;; *)

let maze =
  Random.self_init();
  (* if check_Arg = true *)
  (* then *)
  (*   begin *)
  x := int_of_string(Sys.argv.(1));
  y := int_of_string(Sys.argv.(2));
  start := (Random.int (!x * !y) - 1);
  cell_end := (Random.int (!x * !y) - 1);
  if (!start < 0 || !start > (!x * !y))
  then start := 0;
  listCell := fill_cell !x !y [];
  listDoorV := fill_doorV !x !y [];
  listDoorH := fill_doorH !x !y [];
  listCell := List.rev !listCell;
  listDoorV := List.rev !listDoorV;
  listDoorH := List.rev !listDoorH;
  listCell := putVisited !listCell (List.nth !listCell !start) [];
  (* fill_path !start; *)
  (* print_maze !listCell !listDoorH !listDoorV !x !y 0 0; *)
(*   end *)
(* else *)
  (*   print_endline "./step1 [x > 1] [y > 1]"; *)
;;
  
let main =
  maze
;;
