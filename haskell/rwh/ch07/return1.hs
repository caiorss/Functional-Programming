
-- file: ch07/return1.hs
import Data.Char(toUpper)

isGreen :: IO Bool
isGreen =
  do putStrLn "Is green your favorite color?"
     inpStr <- getLine
     return ((toUpper . head $ inpStr) == 'Y')
