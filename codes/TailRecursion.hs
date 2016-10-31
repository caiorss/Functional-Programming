--Factorial implementation using Tail recursion
fact :: Int -> Int
fact n = facTail n 1
facTail :: (Int) -> (Int) -> (Int)
facTail 0 r = r
facTail n r = facTail (n-1) (n*r)
--Fibonnaci implementation using Tail Recursion
fib :: Int -> Int 
fib n 
	| n == 0  =0
	| n < 0 =0
	| otherwise = fibTail n 1 0

fibTail :: Int -> Int -> Int -> Int
fibTail n result previous
	|n==0 =result
	|otherwise = fibTail (n-1) (result +previous) result
-- Sum of the first Integers	
sumInteger :: Int -> Int
sumInteger n 
		|n < 0 =0
		|n== 0 = 0
		|otherwise = sumTail n 0
		
sumTail :: Int -> Int -> Int
sumTail n result 
	|n == 0 = result
	|otherwise = sumTail (n-1) (n+result)
		
	


