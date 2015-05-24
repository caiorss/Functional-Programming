{-
    From: http://en.wikipedia.org/wiki/Bisection_method

-}
import OldState

bisecStateFactory f = do
    (a, b) <- get
    let c = (a + b) / 2.0
    
    --  If sign(f(c)) = sign(f(a))  else b ← c
    if (f c) * (f a) >= 0   
        then    put (c, b)  -- then a ← c
        else    put (a, c)  -- else b ← c
    
    return c

findRoot :: Double -> Double -> (Double -> Double) -> [Double] -> Maybe Double
findRoot eps itmax f serie =
    case (itmax, serie) of   
        (_, [])          -> Nothing
        (0, (x:xs))      -> if abs(f x) < eps then Just x else Nothing
        (i, (x:xs))      -> if abs(f x) < eps 
                                then Just x
                            else
                                findRoot eps (i - 1) f xs
    
bisecSolver eps itmax f x0 x1 =
    findRoot eps itmax f (evalStateLoop  (bisecStateFactory f) (x0, x1))
    
f :: Double -> Double
f x =  x ** 3.0 - x - 2.0
