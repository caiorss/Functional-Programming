
-- file: ch14/Maybe.hs 

data Maybe a = Nothing | Just a 

instance Monad Maybe where
  
 -- chain 
 (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
 Just x    >>= fn = fn x
 Nothing   >>= fn = Nothing

 -- inject 
 return :: a ->  Maybe a
 return a = Just a 

 ---
 (>>) :: Maybe a -> Maybe b -> Maybe b
 Just _   >> mb = mb
 Nothing  >> mb = Nothing
 
 fail _ = Nothing 



{- Function that executes the Maybe monad. If the computation 
   fails the third parameter is Nothing it returns the value n, 
   on  the other hand if the computation succeeds the third
   parameter is (Just x) it applies the function (a -> b) to the
   value x wrapped in the monad. 

-}
maybe :: b -> (a -> b ) -> Maybe a -> b
maybe n _ Nothing  = n 
maybe _ f (Just x) = f x
