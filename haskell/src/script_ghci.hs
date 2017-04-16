
content <- readFile "/etc/lsb-release"

let nChars = length content

putStrLn $ "File content = \n" ++ content

putStrLn $ "File size (number of chars) = " ++ show nChars

         
putStrLn "--------------------------------"
         
:{
getPassword :: String -> IO () -> IO () -> IO ()         
getPassword password success error = do
  passwd <- putStr "Enter the password: " >> getLine 
  if passwd == password
  then success
  else do error
          getPassword password success error
:}


:{
showSecreteFile :: String -> IO ()
showSecreteFile file = do
  content <- readFile file
  putStrLn "Secrete File Content"
  putStrLn content
   
:}   
  
         
:{
getPassword2 :: String -> Int -> IO ()
getPassword2 password maxTry = aux (maxTry - 1)
  where
    aux counter = do 
      passwd <- putStr "Enter the password: " >> getLine
      case (counter, passwd) of
        _ | passwd == password
              -> showSecreteFile "/etc/shells"
        
        _ | counter == 0
              -> putStrLn $ "File destroyed after " ++ show maxTry ++ " attempts."                 
        _
              -> do putStrLn $ "Try again. You only have " ++ show counter ++ " attempts."
                    aux (counter - 1)                                      
             
:}
   
let printLine = putStrLn "-----------------------------------------------"

printLine                
putStrLn "Open secrete vault: "
getPassword "guessthepassword" (putStrLn "Vault opened. Ok.") (putStrLn "Error: Wrong password. Try Again")               
printLine
putStrLn "Open secret file: (1)"         
getPassword2 "xyzVjkmArchp972343asx" 3

printLine             
putStrLn "Open secret file: (2)"         
getPassword2 "xyzVjkmArchp972343asx" 3
