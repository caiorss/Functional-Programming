{-
Reference: 
    http://pages.pacificcoast.net/~cazelais/187/simpson.pdf:
    http://en.wikipedia.org/wiki/Simpson%27s_rule 

  
 -}
f :: Double -> Double
f x = x

g x = sqrt(1+x^3)


integratorSimple n f a b  = area        
        where 
        dx  = (b - a)/n        
        xi  = map (\i -> a + i*dx) [0..n]
        fxi = map f xi
        area = dx * sum fxi

        
{- Consecutive Points Patterns
 - [(x0, x1), (x1, x2) ... (xk-1, xk)] 
 -}
pairs lst = zip lst (tail lst)  

triples lst = zip3 lst (tail lst) (tail $ tail lst) 



{-   Trapezius Rule Integration
 -
 -}
integratorTrapezius n f a b = area
        where
        dx = (b - a)/n
        xi  = map (\i -> a + i*dx) [0..n]
        fxiPairs = pairs $ map f xi
        area =  1/2*dx * (  sum $ map (\(a, b) -> a+b) fxiPairs)

--{-
integratorSimpson ns f a b = area
        where 
        n = fromIntegral ns
        dx = (b - a)/n
        xi  = map (\i -> a + i*dx) [0..n]
        fxi = map f xi
        area2y = (*2) $ sum $ map (\i -> fxi !! i)  (filter even [1..(ns-1)]) 
        area4y = (*4) $ sum $ map (\i -> fxi !! i)  (filter odd  [1..(ns-1)]) 
        area = dx/3*( area2y + area4y + ( head fxi) + (last fxi))


serieConvergence tol maxit serie = (snd converged, iterations)                    
                    where                    
                    testError = (\(a, b) -> abs(a-b) > tol)
                    sequence = take maxit $ takeWhile testError  (pairs serie) 
                    converged = last sequence
                    iterations = length(sequence)
     

integ1 n = integratorSimple    n g 2 6
integ2 n = integratorTrapezius n g 2 6
integ3 n = integratorSimpson   n g 2 6




