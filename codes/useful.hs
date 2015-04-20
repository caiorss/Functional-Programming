{-
http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2014/lect06.pdf


> search "bookshop" 'o'
[1,2,6]
> 

> let points = [(2, 10), (3, 4), (-1, 3), (2, 2)] ::[(Float, Float)]
> map distance (pairs points)
[6.0827627,4.1231055,3.1622777]
> 
> 

-- Total Distance between points ----
> sum $ map distance $ pairs points
13.368145
> 
> 

> enumerate points 
[(0,(2.0,10.0)),(1,(3.0,4.0)),(2,(-1.0,3.0)),(3,(2.0,2.0))]
> 
> 


-}


enumerate list = zip [0..] list

search :: Eq a => [a] -> a -> [Int]
search xs y = [i | (i, x) <- zip [0..] xs, x==y]

pairs xs = zip xs (tail xs)


{-------------------------------------------------}

-- distance :: Double a => (a, a) -> (a, a) -> a
distance ((x1, y1), (x2, y2)) = sqrt((x2-x1)^2 + (y2-y1)^2)

-- points = [(2, 10), (3, 4), (-1, 3), (2, 2)]
