(*
 #use "topfind" ;;
 #require "unix" ;;
 #load "tools.ml" ;;
*)
let main () =
    Tools.execute_in_dir Sys.argv.(1) "ls"
    |> print_endline

let () = main ()
