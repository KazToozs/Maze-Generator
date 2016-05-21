type cell = {idx : int; mutable visited : bool; isStart : bool; isEnd : bool;}
type door = {mutable isOpen: bool; fstCellIdx : int; scdCellIdx : int}
let rand = ref 0
let cell_end = ref 0
let start = ref 0
let x = ref 0
let y = ref 0
let listDoor = ref [||]
let listCell = ref [||]

let getRightCell pos listCell =
  if ((pos + 1) mod !x != 0)
  then
    begin
      match (listCell.(pos + 1)).visited with
      | false ->
         true
      | true -> false
    end
  else
    false;
;;

let getLeftCell pos listCell =
  if (pos mod !x != 0)
  then
    begin
      match (listCell.(pos - 1)).visited with
      | false ->
       true
      | true -> false
    end
  else
    false;
;;

let getUpCell pos listCell =
  if pos >= !x
  then
    begin
      match (listCell.(pos - !x)).visited with
      | false ->
         true
      | true ->
         false
    end
  else
    false;
;;

let getDownCell pos listCell =
  if pos <= ((!x * !y) - 1 - !x)
  then
    begin
      match (listCell.(pos + !x)).visited with
      | false ->
         true
      | true -> false
    end
  else
    false;
;;

let check_Possibility pos =
  if (getRightCell pos !listCell) = false
     && (getLeftCell pos !listCell) = false
     && (getUpCell pos !listCell) = false
     && (getDownCell pos !listCell) = false
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

let removeLastElem listPath =
  Stack.pop listPath
;;

let putVisited listCell pos =
  listCell.(pos).visited <- true;
;;

let getRightCellR pos listCell =
  if ((pos + 1) mod !x <> 0)
  then
    begin
      match (listCell.(pos + 1)).visited with
      | false -> (match (!listDoor.(pos - (pos / !x))).isOpen with
        | false -> false
        | true -> true)
      | true -> false
    end
  else
    false;
;;

let getLeftCellR pos listCell =
  if (pos mod !x <> 0)
  then
    begin
      match (listCell.(pos - 1)).visited with
      | false -> (match (!listDoor.(pos - 1 - (pos / !x))).isOpen with
        | false -> false
        | true -> true)
      | true -> false
    end
  else
    false;
;;

let getUpCellR pos listCell =
  if pos >= !x
  then
    begin
      match (listCell.(pos - !x)).visited with
      | false -> (match (!listDoor.((pos - !x) + ((!x - 1) * !y))).isOpen with
        | false -> false
        | true -> true)
      | true -> false
    end
  else
    false;
;;

let getDownCellR pos listCell =
  if pos <= ((!x * !y) - 1 - !x)
  then
    begin
      match (listCell.(pos + !x)).visited with
      | false -> (match (!listDoor.((pos + ((!x - 1) * !y)))).isOpen with
        | false -> false
        | true -> true)
      | true -> false
    end
  else
    false;
;;

let rec highlight_path listPath listCell =
    if ((Stack.is_empty listPath) = false) then
      begin
        (* print_int (removeLastElem listPath); *)
        (* print_endline ""; *)
        putVisited listCell (removeLastElem listPath);
        highlight_path listPath listCell;
      end
;;

let check_PossibilityR pos =
  if (getRightCellR pos !listCell) = false
     && (getLeftCellR pos !listCell) = false
     && (getUpCellR pos !listCell) = false
     && (getDownCellR pos !listCell) = false
  then false
  else
    true
;;

let putNotVisited listCell pos =
  listCell.(pos).visited <- false;
;;

let listLen lis = Array.length lis

let rec reset_visited listCell i =
  if i < listLen listCell then
    begin
      putNotVisited listCell i;
      reset_visited listCell (i + 1)
    end
;;

let add_Path pos listPath =
  Stack.push pos listPath
;;

let set_path pos move =
  if move = 1
  then
    begin
      putVisited !listCell (pos + 1);
    end
  else if move = 2
  then
    begin
      putVisited !listCell (pos - 1);
    end
  else if move = 3
  then
    begin
      putVisited !listCell (pos - !x);
    end
  else if move = 4
  then
    begin
      putVisited !listCell (pos + !x);
    end
;;


let rec get_nb count stack =
  match count with
  | 0 -> Stack.pop stack;
  | _ ->
     begin
       Stack.pop stack;
       get_nb (count - 1) stack;
     end
;;

let get_next_move pos stack =
  if (getRightCell pos !listCell = true) then
    Stack.push 1 stack;
  if (getLeftCell pos !listCell = true) then
    Stack.push 2 stack;
  if (getUpCell pos !listCell = true) then
    Stack.push 3 stack;
  if (getDownCell pos !listCell = true) then
    Stack.push 0 stack;
  rand := (get_nb (Random.int (Stack.length stack)) stack);
  Stack.clear stack;
;;

let get_next_moveR pos stack =
  if (getRightCellR pos !listCell = true) then
    Stack.push 1 stack;
  if (getLeftCellR pos !listCell = true) then
    Stack.push 2 stack;
  if (getUpCellR pos !listCell = true) then
    Stack.push 3 stack;
  if (getDownCellR pos !listCell = true) then
    Stack.push 0 stack;
  rand := (get_nb (Random.int (Stack.length stack)) stack);
  Stack.clear stack;
;;

let rec ressolve_path pos listPath =
  if (check_PossibilityR pos) = true then
    begin
      get_next_moveR pos (Stack.create());
      if !rand = 1 then
        begin
          set_path pos 1;
          add_Path (pos + 1) listPath;
          ressolve_path (pos + 1) listPath;
        end
      else if !rand = 2 then
        begin
          set_path pos 2;
          add_Path (pos - 1) listPath;
          ressolve_path (pos - 1) listPath;
        end
      else if !rand = 3 then
        begin
          set_path pos 3;
          add_Path (pos - !x) listPath;
          ressolve_path (pos - !x) listPath;
        end
      else
        begin
          set_path pos 4;
          add_Path (pos + !x) listPath;
          ressolve_path (pos + !x) listPath;
        end
    end
  else if pos <> !start && pos <> !cell_end then
    begin
      if (Stack.is_empty listPath = false) then
        begin
          rand := Stack.top listPath;
          if (check_PossibilityR !rand) = false then
            begin
              ressolve_path (removeLastElem listPath) listPath;
            end
          else
            begin
              ressolve_path !rand listPath;
            end
        end
      else
        ressolve_path !start listPath;
    end
  else
    begin
      reset_visited !listCell 0;
      highlight_path listPath !listCell;
    end
;;

let putOpen listDoor pos =
  listDoor.(pos).isOpen <- true;
;;

let open_door pos move =
  if move = 1
  then
    begin
      putVisited !listCell (pos + 1);
      putOpen !listDoor (pos - (pos / !x));
    end
  else if move = 2
  then
    begin
      putVisited !listCell (pos - 1);
      putOpen !listDoor (pos - 1 - (pos / !x));
    end
  else if move = 3
  then
    begin
      putVisited !listCell (pos - !x);
      putOpen !listDoor ((pos - !x) + ((!x - 1) * !y));
    end
  else if move = 4
  then
    begin
      putVisited !listCell (pos + !x);
      putOpen !listDoor ((pos + ((!x - 1) * !y)));
    end
;;

let rec fill_path pos listPath =
  if (check_Possibility pos) = true
  then
    begin
      get_next_move pos (Stack.create());
      if !rand = 1
      then
        begin
          open_door pos 1;
          add_Path (pos + 1) listPath;
          fill_path (pos + 1) listPath;
        end
      else if !rand = 2
      then
        begin
          open_door pos 2;
          add_Path (pos - 1) listPath;
          fill_path (pos - 1) listPath;
        end
      else if !rand = 3
      then
        begin
          open_door pos 3;
          add_Path (pos - !x) listPath;
          fill_path (pos - !x) listPath;
        end
      else
        begin
          open_door pos 4;
          add_Path (pos + !x) listPath;
          fill_path (pos + !x) listPath;
        end
    end
  else if pos <> !start
  then
    begin
      if (Stack.length listPath > 1) then
        begin
          fill_path (removeLastElem listPath) listPath;
        end
      else
        fill_path !start listPath;
    end
;;

let rec fill_cell limit count l =
  match limit with
  | 0 -> l;
  | _ ->
     begin
       l.(count) <- {idx = count; visited = false; isStart = false; isEnd = false};
       fill_cell (limit - 1) (count + 1) l;
     end
;;

let rec fill_doorV l x b count ref_x =
  match b with
  | 0 -> l
  | _ -> (match x with
          | 1 -> fill_doorV l ref_x (b - 1) count ref_x;
          | _ ->
             begin
               l.(((ref_x - 1) * !y - count - 1)) <-
                 {isOpen = false; fstCellIdx = ((((b - 1) * (ref_x)) + x) - 2);
                  scdCellIdx = (((b - 1) * (ref_x)) + x) - 1};
               fill_doorV l (x - 1) b (count + 1) ref_x;
             end)
;;

let rec fill_doorH l x b count ref_x =
  match b with
  | 1 -> l
  | _ -> (match x with
          | 0 -> fill_doorH l ref_x (b - 1) count ref_x;
          | _ ->
             begin
               l.(count - 1)
               <- {isOpen = false;
                   fstCellIdx = ((((b - 1) * (ref_x)) + x) - ref_x - 1);
                   scdCellIdx = (((b - 1) * (ref_x)) + x) - 1};
               fill_doorH l (x - 1) b (count - 1) ref_x;
             end)
;;

let rec fill_door x y ref_x =
  match !listDoor with
  | [||] ->
     begin
       listDoor := Array.make ((!x - 1) * !y + !x * (!y - 1))
                              {isOpen = false; fstCellIdx = 0; scdCellIdx = 0};
       listDoor := fill_doorV !listDoor !x !y 0 ref_x;
       listDoor := fill_doorH !listDoor !x !y (((!x - 1) * !y) + ((!y - 1) * !x)) !x;
       fill_door x y ref_x;
     end
  | _ -> !listDoor;
;;

let setY a =
  x := a
;;

let setX a =
  y := a
;;

let rec print_first_line count last =
  if (count = last) then
     begin
       print_endline "+";
     end
  else if (count = 0) then
    begin
      print_string "+";
      print_first_line (count + 1) last;
    end
  else
    begin
      print_string "-";
      print_first_line (count + 1) last;
    end
;;

let print_doorH a b arr =
  begin
    if (arr.(a + ((b - 1) * !x) + ((!x - 1) * !y)).isOpen = true)
    then
      print_string "  "
    else
      print_string "--";
  end
;;

let print_doorV a b arr =
  begin
    if (arr.((a + (b * (!x - 1)) - 1)).isOpen = true)
    then
      print_string " "
    else
      print_string "|";
  end
;;

let rec print_inter_line a b arr =
  if a = 0 then
    begin
      print_string "|";
      print_doorH a (b + 1) !listDoor;
      print_inter_line (a + 1) b arr;
    end
  else if a < (!x - 1) then
    begin
      print_string "+";
      print_doorH a (b + 1) !listDoor;
      print_inter_line (a + 1) b arr;
    end
  else if a = (!x - 1) then
    begin
      print_string "+";
      print_doorH a (b + 1) !listDoor;
      print_endline "|";
    end

let rec print_blank_line a b arr =
  if a = 0 then
    begin
      if (!listCell.(a + (b * !x)).isStart = true) then
        print_string "|ST"
      else if (!listCell.(a + (b * !x)).isEnd = true) then
        print_string "|ED"
      else if (!listCell.(a + (b * !x)).visited = true) then
        print_string "|.."
      else
        print_string "|  ";
      print_blank_line (a + 1) b arr;
    end
  else if a < (!x - 1) then
    begin
      print_doorV a b !listDoor;
      if (!listCell.(a + (b * !x)).isStart = true) then
        print_string "ST"
      else if (!listCell.(a + (b * !x)).isEnd = true) then
        print_string "ED"
      else if (!listCell.(a + (b * !x)).visited = true) then
        print_string ".."
      else
        print_string "  ";
      print_blank_line (a + 1) b arr;
    end
  else if a = (!x - 1) then
    begin
      print_doorV a b !listDoor;
      if (!listCell.(a + (b * !x)).isStart = true) then
        print_endline "ST|"
      else if (!listCell.(a + (b * !x)).isEnd = true) then
        print_endline "ED|"
      else if (!listCell.(a + (b * !x)).visited = true) then
        print_endline "..|"
      else
        print_endline "  |";
    end

let rec print_rest a b arr count =
  if b > 1 then
    begin
      print_blank_line 0 count arr;
      print_inter_line 0 count arr;
      print_rest 0 (b - 1) arr (count + 1);
    end
  else if b = 1 then
    begin
      print_blank_line 0 count arr;
      print_first_line 0 (!x * 3);
    end
;;

let print_arr a b arr =
  begin
    print_first_line 0 (!x * 3);
    print_rest a b arr 0;
  end

let rec setCellEnd a b =
  cell_end := (Random.int ((a * b) - 1));
  cell_end := !cell_end - (!cell_end mod a);
  if !cell_end = !start
  then setCellEnd !x !y
;;

let maze () =
  x := 10;
  y := 10;
  rand := 0;
  let arg = [("-r", Arg.Int (setX), "X get");
             ("-c", Arg.Int (setY), "Y get");]
  in Arg.parse arg print_endline "Usage: ./step1 -r [x > 1] -c [y > 1]\n";
     Random.self_init();
     listCell := Array.make (!x * !y) {idx = 0; visited = false; isStart = false; isEnd = false};
     start := (Random.int ((!x * !y) - 1));
     start := !start - (!start mod !x);
     setCellEnd !x !y;
     if !cell_end = !start then
       setCellEnd !x !y;
     listCell := fill_cell (!x * !y) 0 !listCell;
     listDoor := fill_door x y !x;
     !listCell.(!start) <- {idx = !listCell.(!start).idx; visited = true;
                            isStart = true; isEnd = false};
     !listCell.(!cell_end) <- {idx = !listCell.(!cell_end).idx; visited = false;
                            isStart = false; isEnd = true};
     fill_path !start (Stack.create());
     reset_visited !listCell 0;
     putVisited !listCell !start;
     ressolve_path !start (Stack.create());
     putVisited !listCell !start;
     print_arr !x !y !listCell;
;;
