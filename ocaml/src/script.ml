#!/usr/bin/env ocaml 
(**
    
    Note that: there is no main function, entry point in Ocaml lang, 
    a function can run at any point of a program.
*)

#load "unix.cma" ;;
#load "str.cma" ;;

let main () =        
    if (Array.length Sys.argv) = 1 
    then print_endline "No arguments given"
    else (
        print_endline ("The argument is : " ^ Sys.argv.(1)) ;
        print_endline ("All parameter : " ^ String.concat " " (Array.to_list Sys.argv))
    )
        

let () =
    if not !Sys.interactive 
    then main ()   (** If it is being run in batch mode, run main *)
    else ()        (** If ocaml is being run in interactive mode, It doesn't run main *)
