(** 
    To a file be compiled all preprocessor directives like that below
    must be removed or commented like in this file.
    
    #require "unix" ;;
*)
    

let read_channel ch =
       let b = Buffer.create 0 in

       let reader chn =
        try Some (input_line chn)
        with End_of_file -> None

       in let rec aux () =
         match reader ch with
          | None -> ()
          | Some line -> Buffer.add_string b (line ^ "\n") ; aux ()
       in aux () ;
       Buffer.contents b 
       
       
let popen_in cmd =
          let fd = Unix.open_process_in cmd in
          read_channel fd 

(** Execute a command in a directory and return the command output and 
    finally go back to the start directory
*)
let execute_in_dir dir command = 
    let this = Sys.getcwd () in
    Unix.chdir dir ;
    let out = popen_in command in
    Unix.chdir this ;
    out
    
