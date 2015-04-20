{-
*Main> splitstr ',' "hello,world,haskell,is,cool"
["hello","world","haskell","is","cool"]


λ > content <- readFile "Test.csv" 
λ > 
λ > parseCSVcont content
(["C1","C2","C3","C4","C5"],[[1.0,5.0,9.0,13.0,17.0],[2.0,6.0,10.0,14.0,18.0],[3.0,7.0,11.0,15.0,19.0],[4.0,8.0,12.0,16.0,20.0]])
λ > 
λ > 

-}
import Control.Monad

import Network.HTTP

-- Fsharp Operators
(|>) x f = f x
(|>>) x f = map f x
(?>>) x f = filter f x

get url = simpleHTTP (getRequest url) >>= getResponseBody

 
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
                            
-- Split String Routine-                            
splitstr char string = wordsWhen (==char)  string                           

splitLines = splitstr '\n'
splitComma = splitstr ','
splitSemicolon = splitstr ';'

readDouble str = read str :: Double
readInt str = read str :: Int

readDoubleMatrix = map $ map readDouble



parseCsvfile = filedata
    where
    content = readFile "Test.csv"
    lines = liftM splitLines content
    rowsraw  = liftM (map splitComma) lines
    
    header = liftM head rowsraw
    rows   = liftM readDoubleMatrix (liftM tail rowsraw)
    
    filedata = liftM2 zip header rows
    
    
    --show(lines)
    
    
parseCSVcont content = (header, matrix)
    where
    header = content |> splitLines 
                     |>> splitComma 
                     |> head 
    
    matrix = content |>  splitLines 
                     |>> splitComma 
                     |>  tail 
                     |>> map readDouble


main = do
    txt <- readFile "Test.csv"
    let hmatrix = parseCSVcont txt
    
    let header = fst hmatrix
    let matrix = snd hmatrix
    
    print(header)
    print(matrix)
    
