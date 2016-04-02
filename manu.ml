type cell = {idx : int; visited : bool}
type door = {isOpen: bool; fstCellIdx : int; scdCellIdx : int}
let start = ref 0
let x = ref 4
let y = ref 4
let listCell = ref []
let listDoorH = ref []
let listDoorV = ref []
let listPath = ref[]

let rec fill_cell a b =
  match b with
  | 0 -> []
  | _ ->
     (match a with
      | 0 -> fill_cell !x (b - 1);
      | _ -> {idx = a - 1 + (b - 1) * !x; visited = false}::fill_cell (a - 1) b)

let rec fill_doorV a b =
  match b with
  | 0 -> []
  | 1 -> []
  | _ ->
     (match a with
      | 0 -> fill_doorV !x (b - 1)
      | _ -> {isOpen = false; fstCellIdx = ((!x * (b - 1) + a - !x - 1));
             scdCellIdx = ((!x * (b - 1) + a - 1))}::fill_doorV (a - 1) b)
      
let rec fill_doorH a b =
  match b with
  | 0 -> []
  | _ ->
     (match a with
      | 1 -> fill_doorH !x (b - 1)
      | _ -> {isOpen = false; fstCellIdx = ((!x * (b - 1)) + a - 2);
             scdCellIdx = ((!x * (b - 1)) + a - 1)}::fill_doorH (a - 1) b)

let doorIsOpen listDoorH x y x_lim =
  (List.nth listDoorH (x + (y * (x_lim - 1)) - 1)).isOpen;
;;

let doorIsOpenV listDoorH x y x_lim =
  if y = 2
  then
    begin
      (List.nth listDoorH (x + (y * (x_lim - 1)) - 1)).isOpen;
    end
  else
    begin
      (List.nth listDoorH (x + (y * (x_lim - 1)))).isOpen;
    end
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
          if (doorIsOpenV listDoorV (count - x_lim - 1) (y + 1) x_lim) == true
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

(* let getRightCell listDoorH pos listCell = *)
(*   if pos < !x *)
(*   then *)
(*     match (List.nth listCell (pos + 1)).visited with *)
(*     | false -> *)
(*        (match (List.nth listDoorH (pos + 1)).scdCellIdx with *)
(*         | pos -> true *)
(*         | _ -> false) *)
(*     | true -> false *)
(*   else *)
(*     false; *)
(* ;; *)

(* let getLeftCell listDoorH pos listCell = *)
(*   if pos > 1 *)
(*   then *)
(*     match (List.nth listCell (pos - 1)).visited with *)
(*     | false -> *)
(*        (match (List.nth listDoorH (pos)).scdCellIdx with *)
(*         | pos -> true *)
(*         | _ -> false) *)
(*     | true -> false *)
(*   else *)
(*     false; *)
(* ;; *)

(* let getUpCell listDoorH pos listCell = *)
(*   if pos >= !x *)
(*   then *)
(*     match (List.nth listCell (pos - !x)).visited with *)
(*     | false -> *)
(*        (match (List.nth listDoorH (pos - !x)).scdCellIdx with *)
(*         | pos -> true *)
(*         | _ -> false) *)
(*     | true -> false *)
(*   else *)
(*     false; *)
(* ;; *)

(* let getDownCell listDoorH pos listCell = *)
(*   if pos < (!x * !y - !x) *)
(*   then *)
(*     match (List.nth listCell (pos + !x)).visited with *)
(*     | false -> *)
(*        (match (List.nth listDoorH (pos + 1)).scdCellIdx with *)
(*         | pos -> true *)
(*         | _ -> false) *)
(*     | true -> false *)
(*   else *)
(*     false; *)
(* ;; *)

(* let rec putVisited myCell pos =  *)
(*   match myCell with *)
(*   | [] -> [] *)
(*   | head::body -> *)
(*      (match head.idx with *)
(*       | pos -> {idx = head.idx; visited = true}::putVisited body pos; *)
(*       | _ -> putVisited body pos); *)
(* ;; *)

let rec putVisited toMatch listCell =
  match listCell with
  | head::body ->
     if (toMatch.idx = head.idx)
     then {idx = head.idx; visited = true}
     else head::(putVisited toMatch body)
  | [] -> []
;;
  
(* let putOpen listDoorH = {isOpen = true; fstCellIdx = listDoorH.fstCellIdx; scdCellIdx = listDoorH.scdCellIdx}; *)
(* ;; *)

let open_door listDoorH listCell pos move =
  match move with
  | 0 -> []
  | 1 ->
     listCell := putVisited (List.nth listCell (pos + 1)) !listCell;
       (* putOpen (List.nth listDoorH (pos + 1)) *)
     (* (List.nth listCell (pos + 1)).visited := true; *)
     (*     (List.nth listDoorH (pos + 1)).isOpen := true; *)
  (* | 2 -> *)
  (*    begin *)
  (*      putVisited (List.nth listCell (pos - 1)); *)
  (*      (\* putOpen (List.nth listDoorH (pos)); *\) *)
  (*    end *)
  (* | 3 -> *)
  (*    begin *)
  (*      putVisited (List.nth listCell (pos - !x)); *)
  (*      (\* putOpen (List.nth listDoorH (pos - !x)); *\) *)
  (*    end *)
  (* | 4 -> *)
  (*    begin *)
  (*      putVisited (List.nth listCell (pos + !x)); *)
  (*      (\* putOpen (List.nth listDoorH (pos + 1)); *\) *)
  (*    end *)
  | _ -> []
;;

(* let rec fill_path listDoorH listDoorV listCell pos nbTest = *)
(*   match nbTest with *)
(*   | 10 -> [] (\* a revoir *\) *)
(*   | _ -> *)
(*      (match (Random.int 4) with *)
(*       | 1 -> *)
(*          begin *)
(*            if (getRightCell listDoorH pos) == true *)
(*            then *)
(*              begin *)
(*                listDoorH := open_door listDoorH listCell pos 1; *)
(*                fill_path listDoorH listDoorV listCell pos 0; *)
(*              end *)
(*            else *)
(*              fill_path listDoorH listDoorV listCell pos (nbTest + 1); *)
(*          end *)
(*       | 2 -> *)
(*          begin *)
(*            if (getLeftCell listDoorH pos) == true *)
(*            then *)
(*              begin *)
(*                open_door listDoorH listCell pos 2; *)
(*                fill_path listDoorH listDoorV listCell pos 0; *)
(*              end *)
(*            else *)
(*              fill_path listDoorH listDoorV listCell pos (nbTest + 1); *)
(*          end *)
(*       | 3 -> *)
(*          begin *)
(*            if (getUpCell listDoorV pos) == true *)
(*            then *)
(*              begin *)
(*                open_door listDoorH listCell pos 3; *)
(*                fill_path listDoorH listDoorV listCell pos 0; *)
(*              end *)
(*            else *)
(*              fill_path listDoorH listDoorV listCell pos (nbTest + 1); *)
(*          end *)
(*       | 4 -> *)
(*          begin *)
(*            if (getDownCell listDoorV pos) == true *)
(*            then *)
(*              begin *)
(*                open_door listDoorH listCell pos 4; *)
(*                fill_path listDoorH listDoorV listCell pos 0; *)
(*              end *)
(*            else *)
(*              fill_path listDoorH listDoorV listCell pos (nbTest + 1); *)
(*          end) *)
(* ;; *)

let maze =
  Random.self_init();
  x := int_of_string(Sys.argv.(1));
  y := int_of_string(Sys.argv.(2));
  start := Random.int (!x * !y);
  listCell := fill_cell !x !y;
  listDoorV := fill_doorV !x !y;
  listDoorH := fill_doorH !x !y;
  fill_path !listDoorH !listDoorV !listCell !start 0;
  print_maze !listCell !listDoorH !listDoorV !x !y 0 0;
;;
  
let main =
  maze
;;
