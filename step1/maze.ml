let main =
  if (Array.length Sys.argv) = 1 || (Array.length Sys.argv) = 5
  then
    begin
      Generate.maze()
    end
  else 
    print_endline "Usage: ./step1 -r [x > 1] -c [y > 1]";
;;
