{-
http://shuklan.com/haskell/lec08.html#/0/14


> :load inputoutput.hs 
[1 of 1] Compiling Main             ( inputoutput.hs, interpreted )
Ok, modules loaded: Main.
> 
> main
Hello World!!
Haskell Beats the Gang of Four
main is a function of type IO(), it is a impure function
> 
> 

> :t main
main :: IO ()
>
> :l inputoutput.hs 
[1 of 1] Compiling Main             ( inputoutput.hs, interpreted )
Ok, modules loaded: Main.
> 
> prompt 
Prompt: Enter the password
232
Wrong Password
> 
> prompt 
Prompt: Enter the password
12345
Correct! OK
> 



$ runhaskell inputoutput.hs 
Hello World!!
Haskell Beats the Gang of Four
main is a function of type IO(), it is a impure functio

Side-effects are isolated into I/O actions.
Pure code is separated from impure operations.
I/O actions only exist within other I/O actions.

return is a function that "makes an I/O action out of a pure value" *


> untilYexit 
quit the program? y/n
e
not quitting
quit the program? y/n
h
not quitting
quit the program? y/n
s
not quitting
quit the program? y/n
y
> 

> writeTheFile 
Writing the file
Reading the file
Hello world Haskell
> 


-}


main = do
    putStrLn "Hello World!!"
    putStrLn "Haskell Beats the Gang of Four"
    putStrLn "main is a function of type IO(), it is a impure function"


prompt = do 
    putStrLn "Prompt: Enter the password"
    answer <- getLine 
    putStrLn (if answer == "12345" 
              then "Correct! OK" 
              else "Wrong Password")


untilYexit = do
  putStrLn "quit the program? y/n"
  ans <- getLine
  if ans /= "y" then do
    putStrLn "not quitting"
    untilYexit
  else return ()


showFile = do
  theInput <- readFile "Test.csv"
  putStrLn theInput
  
writeTheFile = do
    putStrLn "Writing the file"
    writeFile "test.txt" "Hello world Haskell"
    
    putStrLn "Reading the file"
    theinput <- readFile "test.txt"
    putStrLn theinput 

main2 = putStrLn "Enter name:" >> getLine >>= putStrLn.("Hi " ++)
