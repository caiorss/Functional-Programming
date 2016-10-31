
-- file: ch07/tempfile.hs
import System.IO
import System.Directory    (getTemporaryDirectory, removeFile)

--import System.IO.Error     (catch)
import Control.Exception   (catch, finally, IOException)

-- The main entry point.  Work with a temp file in myAction.
main :: IO ()
main = withTempFile "mytemp.txt" myAction

{- The guts of the program.  Called with the path and handle of a temporary
   file.  When this function exits, that file will be closed and deleted
   because myAction was called from withTempFile. -}
myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
    do -- Start by displaying a greeting on the terminal
       putStrLn "Welcome to tempfile.hs"
       putStrLn $ "I have a temporary file at " ++ tempname

       -- Let's see what the initial position is
       pos <- hTell temph
       putStrLn $ "My initial position is " ++ show pos

       -- Now, write some data to the temporary file
       let tempdata = show [1..10]
       putStrLn $ "Writing one line containing " ++ 
                  show (length tempdata) ++ " bytes: " ++
                  tempdata
       hPutStrLn temph tempdata

       -- Get our new position.  This doesn't actually modify pos
       -- in memory, but makes the name "pos" correspond to a different 
       -- value for the remainder of the "do" block.
       pos <- hTell temph
       putStrLn $ "After writing, my new position is " ++ show pos

       -- Seek to the beginning of the file and display it
       putStrLn $ "The file content is: "
       hSeek temph AbsoluteSeek 0

       -- hGetContents performs a lazy read of the entire file
       c <- hGetContents temph

       -- Copy the file byte-for-byte to stdout, followed by \n
       putStrLn c

       -- Let's also display it as a Haskell literal
       putStrLn $ "Which could be expressed as this Haskell literal:"
       print c


getTempdir :: IO String 
getTempdir =   catch getTemporaryDirectory handler
  where
    handler :: IOException -> IO String
    handler = \ _ -> return "." 


{- This function takes two parameters: a filename pattern and another
   function.  It will create a temporary file, and pass the name and Handle
   of that file to the given function.

   The temporary file is created with openTempFile.  The directory is the one
   indicated by getTemporaryDirectory, or, if the system has no notion of
   a temporary directory, "." is used.  The given pattern is passed to
   openTempFile.

   After the given function terminates, even if it terminates due to an
   exception, the Handle is closed and the file is deleted. -}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do -- The library ref says that getTemporaryDirectory may raise on
       -- exception on systems that have no notion of a temporary directory.
       -- So, we run getTemporaryDirectory under catch.  catch takes
       -- two functions: one to run, and a different one to run if the
       -- first raised an exception.  If getTemporaryDirectory raised an
       -- exception, just use "." (the current working directory).

       {- Note: It doesn't work anymore in Haskell 7.10.2 -}
       {- tempdir <- catch (getTemporaryDirectory) (\_ -> return ".") -}

      --getTempdir :: IO FilePath 
       -- tempdir <- catch getTemporaryDirectory handler
       --   where
       --     handler :: IOException -> IO String
       --     handler = \ _ -> return "." 
       
       tempdir <- getTempdir
       (tempfile, temph) <- openTempFile tempdir pattern 

       -- Call (func tempfile temph) to perform the action on the temporary
       -- file.  finally takes two actions.  The first is the action to run.
       -- The second is an action to run after the first, regardless of
       -- whether the first action raised an exception.  This way, we ensure
       -- the temporary file is always deleted.  The return value from finally
       -- is the first action's return value.
       finally (func tempfile temph) 
               (do hClose temph
                   removeFile tempfile)
