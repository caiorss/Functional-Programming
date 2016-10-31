#use "topfind" ;;
#require "uri" ;;
#require "cohttp.lwt" ;;

let fetch uri =
    let open Lwt in
    Cohttp_lwt_unix.Client.get uri >>= fun (resp, body) ->
        Cohttp_lwt_body.to_string body >>= fun b ->
            Lwt_io.printl b

let fetch_string uri =
    let open Lwt in
    Cohttp_lwt_unix.Client.get uri >>= fun (resp, body) ->
        Cohttp_lwt_body.to_string body 
    
  
let url_content url = 
    let uri = Uri.of_string url 
    in Lwt_main.run (fetch_string uri)
    
let main () = 
    url_content Sys.argv.(1)
    |> print_endline

let () = 
    if !Sys.interactive 
    then ()
    else main ()
