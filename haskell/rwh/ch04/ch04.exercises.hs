
safeHead :: [a] -> Maybe a
safeHead (head:rest) = Just head
safeHead []   = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (head:rest) = Just rest 
safeTail []          = Nothing 


safeLast :: [a] -> Maybe a
safeLast [x] = Just x
safeLast (hd:tl) = safeLast tl 
safeLast []  = Nothing

safeInit :: [a] -> Maybe [a]
safeInit (x:xs)      = Just $ init (x:xs)
safeInit []          = Nothing 

{-
splitWith_aux :: (a -> Bool) -> [a] -> [[a]] -> [[a]]
splitWith_aux fnp []     acc  = [] 
splitWith_aux fnp (x:xs) acc  = if fnp x
                                then  splitWith_aux fnp xs  

-}
