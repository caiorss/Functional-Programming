{-
    recursion.hs    
    Recursive Functions Examples
    
-}

-- Product of A List
prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

-- Factorial
fact 0 = 1
fact n = n*fact(n-1)

-- Fibbonaci
fib 0 = 1
fib 1 = 1
fib n | n>= 2
    = fib(n-1) + fib(n-2)
