
let x = ref 0
let y = ref 0
type case = {x : int; y : int}
let listCase = ref []

let rec fill_case a b =
  if (b != 0)
  then (if (a != 0)
        then {x = a; y = b}::fill_case (a - 1) b
        else fill_case !x (b - 1))
  else []

let main () =
  begin
    x := int_of_string(Sys.argv.(1));
    y := int_of_string(Sys.argv.(2));
    listCase := fill_case !x !y;
  end
;;
