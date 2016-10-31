{-
    Why Do Monads Matter? - London Haskell Group
    https://www.youtube.com/watch?v=3q8xYFDYLeI

-}


divby :: Integral a => a -> a -> a
divby x y = div y x

main = do 
            print $ divby 2 . divby 5  $  2310
            print $ divby 3 . divby 7  $  2310
            print $ divby 5 . divby 11 $  2310


main2 = do 
            print $ divby 2 . divby 5  $  2310
            print $ divby 0 . divby 7  $  2310
            print $ divby 5. divby 11 $  2310

data Err a = OK a | Error
            deriving Show

{-
    divbysafe :: Integral a => a -> a -> Err a
    
    *Main> divbysafe 0 10
    Error
    *Main> divbysafe 9 10
    OK 1
    *Main> divbysafe 8 10
    OK 1
    *Main> 

-}
divbysafe 0 y = Error
divbysafe x y = OK(div y x)

{-
    *Main> divbysafe 2 `composeErr` divbysafe 5 $ 10
    OK 1
    *Main> divbysafe 2 `composeErr` divbysafe 0 $ 10
    Error
    *Main> divbysafe 0 `composeErr` divbysafe 5 $ 10
    Error
    *Main> 
-}
composeErr f g x  = case g x of
                   OK y  -> f y
                   Error -> Error
                   
idErr :: a -> Err a
idErr x = OK x


safediv 0 y = Nothing
safediv x y = Just(y/x)
