{-
    Quick Check Examples:
    
    λ> quickCheck prop_revapp2
    +++ OK, passed 100 tests.
    λ> 

    λ> quickCheck prop_revapp
    *** Failed! Falsifiable (after 2 tests and 1 shrink):     
    [1]
    [0]
    λ> 
        
-}

import Test.QuickCheck
import Data.List


{-

    https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing
-}
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

prop_revapp2 :: [Int] -> [Int] -> Bool
prop_revapp2 xs ys = reverse (xs++ys) == reverse ys ++ reverse xs 

{- Split string at character -}
split c [] = []
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs
          
joinstr :: Char -> [String] -> String
joinstr c xs = concat (intersperse [c] xs)


prop_joinstr c xs = 
    joinstr c (split c xs) == xs


main = quickCheck prop_revapp
