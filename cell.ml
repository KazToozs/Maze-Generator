
type cell = (bool * int * int)
type graph = (cell * cell list) list

let create b x y = (b, x, y)

let make_array x y cell = Array.make_matrix x y cell

let create_graph maze x y =
  |  ->
  |


let main = create_graph make_array (int_of_string (Sys.argv.1) int of string Sys.argv.2) int_of_string Sys.argv.1 int_of_string Sys.argv.2
main;;
