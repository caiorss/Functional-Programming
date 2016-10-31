-- https://cseweb.ucsd.edu/classes/wi12/cse230-a/lectures/parsers.html
--
--  
--
--

import Data.Char
import Data.Functor
import Control.Monad

--type ST a = State -> (a, State)

newtype Parser a = P (String -> [(a, String)])

returnP x = P (\cs -> [(x, cs)])

bindP p1 fp2 = P (\cs -> 
    [(y, cs'') | (x, cs')  <- doParse p1 cs
               , (y, cs'') <- doParse (fp2 x) cs'])

instance Monad Parser where
   (>>=)  = bindP
   return = returnP

doParse (P p) s = p s


char c = satP (== c)
string = mapM char

pairP px py = do x <- px
                 y <- py
                 return (x, y)

failP = P $ const []

satP ::  (Char -> Bool) -> Parser Char
satP p = do 
  c <- oneChar 
  if p c then return c else failP


chooseP p1 p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

lowercaseP = satP isAsciiLower
alphaChar = satP isAlpha
digitChar = satP isDigit
alphaNumChar = alphaChar `chooseP` digitChar

digitInt  = do 
  c <- digitChar
  return ((read [c]) :: Int)



grabn :: Int -> Parser String 
grabn n | n <= 0    = return ""
        | otherwise = do c  <- oneChar  
                         cs <- grabn (n-1)
                         return (c:cs)

oneChar :: Parser Char
oneChar = P (\cs -> case cs of
               c:cs' -> [(c, cs')]
               _     -> [])

twoChar0 = P (\cs -> case cs of
                 c1:c2:cs' -> [((c1,c2), cs')]
                 _         -> [])

twoChar = pairP oneChar oneChar 


{------------------------------------------------------------------------------
Even with the rudimentary parsers we have at our disposal, we can start 
doing some rather interesting things. For example, here is a little calculator. 
First, we parse the operation

-}
intOp = plus `chooseP` minus `chooseP` times `chooseP` divide 
  where plus   = char '+' >> return (+)
        minus  = char '-' >> return (-)
        times  = char '*' >> return (*)
        divide = char '/' >> return div




calc = do x  <- digitInt
          op <- intOp
          y  <- digitInt 
          return $ x `op` y


--twoChar9 = oneChar `pairP` oneChar
