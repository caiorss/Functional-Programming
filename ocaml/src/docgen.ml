#!/usr/bin/env ocaml
(** 

Author: Caio Rodrigues
   
Description:

   This ocaml script creates html ocaml documentation 
   of any installed package.


*)

#use "topfind" ;;
               
(* Load Unix and Str builtin libraries *)                             
#load "unix.cma" ;;
#load "str.cma" ;;
#require "pcre" ;;
  
let flatten xs =
  List.fold_right (@) xs []
  
let concat_path pathlist =
  let path = List.fold_right Filename.concat pathlist "" in
  Filename.concat (Filename.dirname path) (Filename.basename path)
  
  
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

let read_file fname =
  let fd = open_in fname in
  read_channel fd
                   
                       
let popen cmd =
  let fd = Unix.open_process_in cmd in
  read_channel fd 
;;

let file_extension fname =
  Str.split (Str.regexp "\\.") fname
  |> fun lst -> List.nth lst ((List.length lst) - 1)

let is_ftype ext filename =
  file_extension filename = ext
                         
let listdir path =
  Sys.readdir path
  |> Array.to_list
  |> List.map (fun fname -> Filename.concat path fname)

let mkdir dirname =
  try
    Unix.mkdir dirname 0o777
  with _ -> ()
                                        
              
let left_pos s len =
  let rec aux i =
    if i >= len then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (succ i)
    | _ -> Some i
  in
  aux 0

let right_pos s len =
  let rec aux i =
    if i < 0 then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (pred i)
    | _ -> Some i
  in
  aux (pred len)

let trim s =
  let len = String.length s in
  match left_pos s len, right_pos s len with
  | Some i, Some j -> String.sub s i (j - i + 1)
  | None, None -> ""
  | _ -> assert false


module Package =
  struct

    let libpath  =
      popen "ocamlfind printconf path"
      |> trim

    let baselib  =
      Filename.concat libpath  "ocaml"

    let list_packages () =
      libpath
      |> listdir
      |> List.map Filename.basename
    
    (** Get package Directory  given the package name

     *)                
    let getdir package_name =
      Filename.concat libpath package_name
      (* popen @@ "ocamlfind query " ^ package_name *)
      |> trim
    ;;       

    (** Get all interface files from a given package 
     *)      
    let interface_files dirname =   
      dirname
      |> getdir 
      |> listdir 
      |> List.filter (is_ftype "mli")
    ;;

    let list () =
      ignore (Sys.command "ocamlfind list")


    let read_meta_file package_name =               
      getdir package_name 
      |> fun x -> Filename.concat x "META"
      |> read_file                 

                         
    let depencies package_name =
      try 
        read_meta_file package_name 
        |>  Pcre.extract_all ~pat:"requires\\s+=\\s*\"(.*)\""
        |>  Array.to_list 
        |>  List.map (fun a -> a.(1))
        |>  List.map (Str.split (Str.regexp "[,| ]"))
        |>  flatten
      with _ -> []

    let make_doc package_name =
      let packdir = getdir package_name in

      (* Path of dependencies *)
      let deps_path =
        depencies package_name 
        |> List.map getdir 
        |> List.map (fun x -> "-I " ^ x) 
        |> String.concat " "        
      
     in  mkdir package_name ;
           
     let cmd =
      package_name
      |> interface_files
      |> String.concat " "  
      |> fun files ->
         (Printf.sprintf
          "ocamldoc  -sort -keep-code -thread -colorize-code -I %s %s -html -d %s -I %s %s "
          baselib  deps_path package_name packdir files
         )
           
     in print_endline cmd ;
        ignore (Sys.command cmd )
           

    (** Build the documentation and open browser in the 
        generated documentation 
    *)            
    let browser_doc package_name =
       let path = concat_path [Sys.getcwd () ;  package_name ; "index.html"] in
       let open_browser () = ignore (Sys.command ("chromium-browser " ^ path)) in

       print_endline path;

       if not (Sys.file_exists (Filename.concat (Sys.getcwd ()) package_name))
       then ( make_doc package_name ; open_browser ())
       else open_browser ()          

    let make_all_docs () =
      List.iter make_doc (list_packages ())

  end;;


  
  
let main () =
  if Array.length Sys.argv = 1
  then (
    print_endline
        "
Ocaml docgen wrapper to ocamlfind 
                  
        Usage:                                   
                                                 
        List all packages               
        $ ./docgen.ml -list                      
                                                 
        Generate package documentation           
        $ ./docgen.ml -doc <name of package>    

        Generate the documentation of all packages
        $ ./docgen.ml -doc all

        Get Package Directory
        $ ./docgen.ml -dir <name of package> 

         Generate package documentation and open index.html in the browser.
        $ ./docgen.ml -browser <name of package>

        

        " ;
  )
  else (
    match Sys.argv.(1) with
    | "-list"    -> Package.list ()
    | "-dir"     -> print_endline (Package.getdir Sys.argv.(2))
    | "-doc"     -> if Sys.argv.(2) <> "all"
                    then  Package.make_doc Sys.argv.(2)
                    else  Package.make_all_docs () 
    | "-browser" -> Package.browser_doc Sys.argv.(2)
    | _          -> print_endline "Error: Invalid command" 

  )

let () =
  if not !Sys.interactive
  then main ()
  else () 
