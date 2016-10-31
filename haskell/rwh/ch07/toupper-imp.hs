
-- file: ch07/toupper-imp.hs
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
  inh <- openFile "/etc/issue" ReadMode
  outh <- openFile "/tmp/issue.out" WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
  do ineof <- hIsEOF inh
     if ineof
       then return ()
       else do inpStr <- hGetLine inh
               hPutStrLn outh (map toUpper inpStr)
               mainloop inh outh
               
