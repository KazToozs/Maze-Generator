type path = {prevMove : int}
type cell = {idx : int; visited : bool; isStart : bool; isEnd : bool}
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
          | _ -> fill_cell (a - 1) b ({idx = a - 1; visited = false; isStart = false; isEnd = false}::l)
         )
  | _ -> (match a with
          | 0 -> fill_cell !x (b - 1) (l);
          | _ -> fill_cell (a - 1) b ({idx = (a - 1) + (b - 1) * !x; visited = false; isStart = false;
                                      isEnd = false}::l)
         )

let rec fill_doorV a b l = match b with
  | 1 -> l;
  | _ -> (match a with
          | 0 -> fill_doorV !x (b - 1) (l);
          | _ -> fill_doorV (a - 1)
                            b
                            ({isOpen = false; fstCellIdx = ((!x * (b - 1) + a - 1));
                              scdCellIdx = ((!x * (b - 1) + a))}::l)
         )

let rec fill_doorH a b l = match b with
  | 0 -> l;
  | _ -> (match a with
          | 1 -> fill_doorH !x (b - 1)
                            ({isOpen = false; fstCellIdx = (a + !x * (b - 1));
                              scdCellIdx = (a + !x * b)}::l);
          | _ -> fill_doorH (a - 1) b
                            ({isOpen = false; fstCellIdx = (a + !x * (b - 1));
                              scdCellIdx = (a + !x * b)}::l);
         )

let doorIsOpen listDoorH x y x_lim =
  (List.nth listDoorH (x + (y * (x_lim - 1)) - 1)).isOpen;
;;

let doorIsOpenV listDoorV x y x_lim =
  (List.nth listDoorV (x + ((y - 1) * x_lim))).isOpen;
;;

let isStart x y listCell x_lim =
  (List.nth listCell (x + (y * x_lim))).isStart;
;;

let isEnd x y listCell x_lim =
  (List.nth listCell (x + (y * x_lim))).isEnd;
;;

let print_start_or_end count y listCell =
  if (((isStart count y listCell !x) = false) && (isEnd count y listCell !x) = false)
  then
    begin
      print_string "|  ";
    end
  else if (isStart count y listCell !x) = true
  then
    begin
      print_string "|ST";
    end
  else if (isEnd count y listCell !x) = true
  then
    begin
      print_string "|ED";
    end
;;

let rec print_doorH listDoorH count x x_lim y y_lim =
  if ((x + (y * (x_lim - 1)) - 1) < (x_lim * y_lim))
  then
    begin
      if count = 0
      then
        begin
          print_start_or_end x y !listCell;
          print_doorH listDoorH (count + 1) x x_lim y y_lim
        end
      else if count != (x_lim)
      then
        begin
          if (doorIsOpen listDoorH count y x_lim) = true
          then
            begin
              if isStart count y !listCell x_lim = false && isEnd count y !listCell x_lim = false
              then
                begin
                  print_string "   ";
                end
              else if isStart count y !listCell x_lim = true
              then
                begin
                  print_string " ST";
                end
              else
                print_string " ED";
              print_doorH listDoorH (count + 1) x x_lim y y_lim
            end
          else
            begin
              print_start_or_end count y !listCell;
              print_doorH listDoorH (count + 1) x x_lim y y_lim
            end
        end
      else if count = (x_lim)
      then
        begin
          print_char '|';
        end
    end
;;

let rec print_Vdoor listDoorV count x x_lim y y_lim=
  if ((x + (y * (x_lim - 1))) < (x_lim * y_lim)) && y < y_lim
  then
    begin
      if count = 0
      then
        begin
          print_string "\n|";
          print_Vdoor listDoorV (count + 1) x x_lim y y_lim
        end
      else if count < (x_lim)
      then
        begin
          if (doorIsOpenV listDoorV (count - 1) y x_lim) == true
          then
            begin
              print_string "  +";
              print_Vdoor listDoorV (count + 1) x x_lim y y_lim
            end
          else
            begin
              print_string "--+";
              print_Vdoor listDoorV (count + 1) x x_lim y y_lim
            end
        end
      else if count = (x_lim)
      then
        begin
          if (doorIsOpenV listDoorV (count - 1) y x_lim) == true
          then
            begin
              print_string "  |";
              if count = (x_lim)
              then
                print_char '\n';
              print_Vdoor listDoorV (count + 1) x x_lim y y_lim
            end
          else
            begin
              print_string "--|";
              if count = (x_lim)
              then
                print_char '\n';
              print_Vdoor listDoorV (count + 1) x x_lim y y_lim
            end
        end
    end
;;

let rec print_last_line count x_lim =
  if count != 0
  then
    begin
      if count = 1
      then
        begin
          print_string "--+\n";
          print_last_line (count - 1) x_lim;
        end
      else if count = x_lim
      then
        begin
          print_string "\n+---";
          print_last_line (count - 1) x_lim;
        end
      else
        begin
          print_string "---";
          print_last_line (count - 1) x_lim;
        end
    end

let rec print_first_line count =
  if count = 0
  then
    begin
      print_string "+--";
      print_first_line (count + 1)
    end
  else if (count + 1) = !x
  then
    begin
      print_string "---+\n";
    end
  else
    begin
      print_string "---";
      print_first_line (count + 1)
    end
;;

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
               print_first_line 0;
               print_doorH listDoorH 0 x x_lim y y_lim;
               print_maze body listDoorH listDoorV x_lim y_lim 0 (y + 1)
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

let rec putVisited listCell toMatch l =
  match listCell with
  | head::body ->
     if (toMatch.idx = head.idx)
     then putVisited body toMatch ({idx = head.idx; visited = true; isStart = head.isStart;
                                   isEnd = head.isEnd}::l)
     else putVisited body toMatch (head::l)
  | [] -> List.rev l
;;

let rec putOpen listDoor toMatch l =
  match listDoor with
  | head::body ->
     if (toMatch.fstCellIdx = head.fstCellIdx && toMatch.scdCellIdx = head.scdCellIdx)
     then putOpen body toMatch ({isOpen = true; fstCellIdx = head.fstCellIdx; scdCellIdx = head.scdCellIdx}::l)
     else putOpen body toMatch (head::l)
  | [] -> List.rev l
;;

let open_door pos move =
  if move = 1
  then
    begin
      listCell := putVisited !listCell (List.nth !listCell (pos + 1)) [];
      listDoorH := putOpen !listDoorH (List.nth !listDoorH (pos - (pos / !x))) [];
    end
  else if move = 2
  then
    begin
      listCell := putVisited !listCell (List.nth !listCell (pos - 1)) [];
      listDoorH := putOpen !listDoorH (List.nth !listDoorH (pos - (pos / !x) - 1)) [];
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
      rand := getPrevMove !listPath;
      listPath := removeLastElem !listPath;
      fill_path !rand;
    end
;;

let rec print_list listCell =
  match listCell with
  | [] -> [];
  | head::body ->
     if (head.isStart = true)
     then
       print_endline "start";
     print_list body;
;;

let setY a =
  x := a

let setX a =
  y := a


let rec putStart listCell toMatch l =
  match listCell with
  | head::body ->
     if (toMatch.idx = head.idx)
     then putStart body toMatch ({idx = head.idx; visited = false; isStart = true;
                                   isEnd = head.isEnd}::l)
     else putStart body toMatch (head::l)
  | [] -> List.rev l
;;

let rec putEnd listCell toMatch l =
  match listCell with
  | head::body ->
     if (toMatch.idx = head.idx)
     then putEnd body toMatch ({idx = head.idx; visited = false; isStart = head.isStart;
                                   isEnd = true}::l)
     else putEnd body toMatch (head::l)
  | [] -> List.rev l
;;

let maze () =
  x := 10;
  y := 10;
  let arg = [("-r", Arg.Int (setX), "X get");
             ("-c", Arg.Int (setY), "Y get");]
  in Arg.parse arg print_endline "Usage: ./step1 -r [x > 1] -c [y > 1]\n";
     Random.self_init();
     start := (Random.int ((!x * !y) - 1));
     cell_end := (Random.int ((!x * !y) - 1));
     if !cell_end = !start
     then
       cell_end := (Random.int (!x * !y) - 1);
     if (!start < 0 || !start > (!x * !y))
     then start := 0;
     listCell := fill_cell !x !y [];
     listDoorV := fill_doorV !x !y [];
     listDoorH := fill_doorH !x !y [];
     listCell := List.rev !listCell;
     listDoorV := List.rev !listDoorV;
     listDoorH := List.rev !listDoorH;
     listCell := putStart !listCell (List.nth !listCell !start) [];
     listCell := putEnd !listCell (List.nth !listCell !cell_end) [];
     listCell := putVisited !listCell (List.nth !listCell !start) [];
     fill_path !start;
     print_maze !listCell !listDoorH !listDoorV !x !y 0 0;
;;