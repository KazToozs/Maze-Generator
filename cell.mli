type cell
type graph = (cell * cell list) list

val create : bool -> int -> int -> cell
