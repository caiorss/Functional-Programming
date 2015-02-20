{- 
 -
 - http://www.fatvat.co.uk/2009/10/dealing-with-partial-functions.html 
 -   
 - Or using Maybe to indicate (via the type system) that the function 
 - might not return a value. The first exercise of chapter 4 of Real World 
 - Haskell asks you to rewrite some of the partial list functions so that they 
 - never fail. 
 - 
 -
 -


-}



fromJust (Just x) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (y:[]) = Just y
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just (x : fromJust(safeInit xs))

--safediv y 0 = Nothing
--sefediv y x = Just(y/x)

safediv y x | x == 0    = Nothing
            | otherwise = Just(y/x)

