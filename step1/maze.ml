let main =
  if (Array.length Sys.argv) = 1 || (Array.length Sys.argv) = 5
  then
    Generate.maze()
  else
    print_endline "Usage: ./step1 [x > 1] [y > 1]";
;;
