{-
    recursion.hs    
    Recursive Functions Examples
    
    * http://www.fh-wedel.de/~si/HXmlToolbox/thesis/x298.html
-}


-- Calculate the length of a list
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs
            

-- Product of A List
prod :: [Int] -> Int
prod []     = 1
prod (x:xs) = x * prod xs

-- Reverse a List
reverse2 :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse2 xs ++ [x]

-- Maximum Element of a list
-- maxx :: [a] -> a
-- maxx [x]    = x
-- maxx (x:xs) | x >= maxx(xs) = x | otherwise maxx(xs)

-- Factorial
fact 0 = 1
fact n = n*fact(n-1)

-- Fibbonaci
fib 0 = 1
fib 1 = 1
fib n | n>= 2
    = fib(n-1) + fib(n-2)
    
data Vect = Scalar Double | 
            Vector [Double]
            

-- vectorize :: Vect b => (b -> b) -> b -> b
-- vectorize func Scalar = func Scalar
-- vectorize func Vector = map func Vector


map2 func x      = func x
map2 func (x:xs) = func map (x:xs)




