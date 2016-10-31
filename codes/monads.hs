{-
    http://adit.io/posts/2013-06-10-three-useful-monads.html

-}


half x = x / 2

half2 x = (x `div` 2, "I just halved " ++ (show x) ++ "!")

-- Writer Monad
data Writer w a = Writer { runWriter :: (a, w) }  

{-
half3 :: Int -> Writer String Int
half3 x = do
        print ("I just halved " ++ (show x) ++ "!")
        return (x `div` 2)

-}

--  This function takes a value and reuturns a wrapped value
--
--  http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
--  Suppose half is a function that only works on even numbers: 
--
--  halfeven :: Integral a => a -> Maybe a
--
--  *Main> Just 20 >>= halfeven  >>= halfeven  >>= halfeven 
-- Nothing
-- *Main> 
--  
--      Maybe: is a Functor, an Applicative, and a Monad.
--      
--
{-
http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html


Conclusion

    A functor is a data type that implements the Functor typeclass.
    An applicative is a data type that implements the Applicative typeclass.
    A monad is a data type that implements the Monad typeclass.
    A Maybe implements all three, so it is a functor, an applicative, and a monad


    functors: you apply a function to a wrapped value using fmap or <$>
    applicatives: you apply a wrapped function to a wrapped value using <*> or liftA
    monads: you apply a function that returns a wrapped value, to a wrapped value using >>= or liftM


-}
halfeven x = if even x
           then Just (x `div` 2)
           else Nothing
