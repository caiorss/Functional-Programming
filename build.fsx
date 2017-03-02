// #!/usr/bin/env fsi

// F# - Fsharp build script to export org-files to html

open System

type P = System.IO.Path 
type SF = System.IO.File 

/// Get directory files 
let getFiles path ext = System.IO.Directory.GetFiles(path, ext)

/// Change file extension 
let changeExt ext filePath = P.ChangeExtension(filePath, ext)

/// Get current directory 
let getCwd () = System.IO.Directory.GetCurrentDirectory()

let putStrLn s = printfn "%s" s

/// Try run fn () anyway regardless of the error.
let doAnyway fn =
    try fn ()
    with _ -> ()


let moveFile fileName dest =
    if System.IO.File.Exists dest 
    then System.IO.File.Delete dest
    System.IO.File.Move(fileName, dest)
    

let runShell executable (arglist: string list) : int =
    let args = String.Join(" ", arglist)

    // printfn "%s" args 
    
    let psi = new System.Diagnostics.ProcessStartInfo(executable, args)
    psi.RedirectStandardOutput <- true 
    psi.RedirectStandardError  <- true
    psi.RedirectStandardInput  <- true 
    psi.UseShellExecute        <- false
    psi.CreateNoWindow         <- true
    
    use proc = new System.Diagnostics.Process(StartInfo = psi)

    ignore <| proc.OutputDataReceived.Subscribe(fun evt -> Console.WriteLine(evt.Data))
    ignore <| proc.ErrorDataReceived.Subscribe (fun evt -> Console.WriteLine(evt.Data))

    ignore <| proc.Start()
    proc.StandardInput.AutoFlush <- false 
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()

    // while true do
    //     proc.StandardInput.Write(Console.Read())
    
    proc.WaitForExit()
    proc.ExitCode

/// Export org-mode file  file.org to file.html
///    
let exportFileMakeHtml orgFile elispScript =    
    ignore <| runShell "emacs" [ orgFile
                        // Load build.el settings
                       ; "-l" ; elispScript
                       // Don't load init.el
                       ; "-q"        
                       // Run in batch mode
                       ; "--batch"   
                       // Run function (org-html-export-to-html)  
                       // "-f" ; "org-html-export-to-html"
                       ; "-f" ; "export-to-html"                                 
                       
                       // Exit emacs after file is processed
                       ; "--kill"
                       ]  

let exportFile dest destRelPath orgFile =
    let elispScript = P.Combine [| getCwd (); "build.el" |]
    exportFileMakeHtml orgFile elispScript ;

    let htmlFileOrig = changeExt ".html" orgFile
    
    let htmlFileDest = P.Combine [| dest;
                                destRelPath;
                                P.GetFileName <| changeExt ".html" orgFile |]
    printfn "Exporting file %s to %s" orgFile htmlFileDest;

    try
        // SF.Copy(htmlFileOrig, htmlFileDest, true)
        moveFile htmlFileOrig htmlFileDest
        printfn "Ok."
    with :? System.IO.FileNotFoundException as ex -> printfn "Error: %s" ex.Message
    
    

let exporDirectory dest destRelPath  =
    getFiles destRelPath "*.org"
    |> Array.iter (exportFile dest destRelPath)



// exporDirectory "dist" "haskell"        

let build () =
    exporDirectory "dist" "."
    doAnyway (fun () -> SF.Move("dist/README.html", "dist/index.html"))
    let exportDirs = [ "ocaml"; "clojure"; "papers"; "scheme"; "haskell"]
    List.iter (exporDirectory "dist") exportDirs


let main () =
    getFiles "haskell" "*.org"
    |> Array.iter putStrLn



build ()


