{-
Numerical Methods in Functional Programming

    Features:
        1 - Root Solving
        2 - Derivate Function
        4 - Horner Method to evaluate polynomia.

-}
import Data.Maybe


-- Pipe Operators
--
(|>) x f = f x
(|>>) x f = map f x

{-

-}



derivate :: Fractional a => a -> (a -> a) -> a -> a
derivate dx f x = (f(x+dx) - f(x))/dx



{-
    Example: Evaluating Polynomia by the horner method.

Reference: http://www.math10.com/en/algebra/horner.html

f(x) = a0 + a1x + a2x2 + a3x3 + a4x4 + a5x5
f(x0) = a0 + x0(a1 + x0(a2 + x0(a3 + x0(a4 + a5x0)))) 

Example: Evaluate the polynomial 
    f(x)  =  1x4 + 3x3 + 5x2 + 7x + 9 at x = 2 
    df(x) =  3x3 + 6x2 + 10x +  7
    
let coeffs  = [9.0, 7.0, 5.0, 3.0, 1.0] 
let f  = polyval  coeffs

let df = polyval $  polyderv coeffs

> polyderv coeffs 
[7.0,10.0,9.0,4.0

> f 2
83.0

> df 2
95.0

> (\x -> 7 + 10*x + 9*x^2 + 4*x^3) 2
95


-}
polyval :: Fractional a => [a] -> a -> a
polyval coeffs x = foldr (\b c -> b + x*c) 0 coeffs
    -- where
    -- hx x b c = b + x*c

{-
    Polynomia derivate
    
    Creates the polynomia of the derivate of the input
    polynomia.
    
    > polyderv [9.0, 7.0, 5.0, 3.0, 1.0]
    [7.0,10.0,9.0,4.0]
    
-}
polyderv :: Fractional a => [a] -> [a] 
polyderv coeffs = zipWith (*) (map fromIntegral [1..n]) (tail coeffs )
    where
    n = (length coeffs) - 1 
    -- |>> fromIntegral
    -- poly = zipWith (*) ([1..n] |>> fromIntegral) (tail coeffs)
    




{-


*Main> let f x  =  exp(-x) -3*log(x)
*Main> bissection_solver 1e-5  100 f 0.1 3
(1.1154491424560549,3.395150475205e-6,18)
*Main> 

-}

bissection_iterator :: (Floating a, Floating a1, Ord a1) => (a -> a1) -> [a] -> [a]
bissection_iterator f guesslist = newguess
    where
    a =  guesslist !! 0
    b =  guesslist !! 1
    c = (a+b)/2.0
    p = f(a)*f(c)
    newguess = (\p -> if p < 0.0 then [a, c] else [c, b] ) p


bissectionSolver eps itmax f x1 x2 = (root, error, iterations) 
    where  
    
    bissection_error xlist = abs(f $ xlist !! 1)
    check_error xlist = bissection_error xlist > eps

    iterator = bissection_iterator  f

    rootlist = [x1, x2] |> iterate iterator |> takeWhile check_error |> take itmax

    pair = last rootlist |> iterator
    root = last pair
    error = bissection_error pair

    iterations = length rootlist    

-- let f x  =  exp(-x) -3*log(x)
-- let itf  = bissection_iterator f     


secant_iterator :: Floating t => (t -> t) -> [t] -> [t]
secant_iterator f guesslist = [x, xnext]
    where
    x =  guesslist !! 0
    x_ = guesslist !! 1
    xnext = x - f(x)*(x-x_)/(f(x) - f(x_))

secantSolver eps itmax f x1 x2 = (root, error, iterations) 
    where  
    
    secant_error xlist = abs(f $ xlist !! 1)
    check_error xlist = secant_error xlist > eps

    iterator = secant_iterator  f

    rootlist = [x1, x2] |> iterate iterator |> takeWhile check_error |> take itmax

    pair = last rootlist |> iterator
    root = last pair
    error = secant_error pair

    iterations = length rootlist


{-
Newton-Raphson Method Iterator, builds an iterator function
fromt the function to be solved and its derivate.

-}
newton_iterator f df x = x - f(x)/df(x)

{---------------------------------------------------------------------
    newtonSolver(eps, itmax, f, df, guess)

    Solve equation using the Newton-Raphson Method.
    
    params:
    
        eps   :  Tolerance of the solver
        itmax :  Maximum number of iterations
        f     :  Function which the root will be computed
        df    :  Derivate of the function
        guess :  Initial guess 

newtonSolver
  :: (Fractional t, Ord t) =>
     t -> Int -> (t -> t) -> (t -> t) -> t -> (t, t, Int)
-----------------------------------------------------------------------
-}
newtonSolver :: (Floating t, Ord t) => t -> Int -> (t -> t) -> (t -> t) -> t -> (t, t, Int)
newtonSolver eps itmax f df guess = (root, error, iterations)
    where
    check_root x = abs(f(x)) > eps                                  
    iterator = newton_iterator f df   -- Builds the Newton Iterator                              
    generator = iterate $ iterator    -- Infinite List that will that holds the roots (Lazy Evaluation)

    rootlist = take itmax $ takeWhile check_root $ generator guess                                  
    root = iterator $ last $ rootlist                                  
    error = abs(f(root))
    iterations = length rootlist




{-

*Main> let f x = exp(x) - 3.0*x^2
*Main> 
*Main> steffensenSolver 1e-5 100 f 4
(3.733079203355024,3.3912123882373635e-6,54)
*Main> 

*Main> f 3.733079203355024
3.3912123882373635e-6
*Main

-}
steffensen_iterator :: (Floating a) => (a -> a) -> a -> a
steffensen_iterator f x = x - y^2/( f(x+y) -f(x))
    where
    y = f x

steffensenSolver :: (Floating t, Ord t) => t -> Int -> (t -> t) -> t -> (t, t, Int)
steffensenSolver eps itmax f guess = (root, error, iterations)
    where
    check_root x = abs(f(x)) > eps                                  
    iterator = steffensen_iterator f    -- Builds the Newton Iterator                              
    generator = iterate iterator    -- Infinite List that will that holds the roots (Lazy Evaluation)

    rootlist = take itmax $ takeWhile check_root $ generator guess                                  
    root = iterator $ last $ rootlist                                  
    error = abs(f(root))
    iterations = length rootlist



newton2Solver eps itmax f guess = newtonSolver eps itmax f df guess
    where 
    df = derivate 1e-8 f



square_root a | a > 0       = newtonSolver 1e-6 50 (\x -> x^2 -a) (\x -> 2*x) a 
              | otherwise   = error ("The argument must be positive")


{-
Linear Interpolation

*Main> let rows2col rows idx = table |>> (\row -> row !! idx)
*Main> 
*Main> rows2col table 0
[1950.0,1960.0,1970.0,1980.0,1990.0]
*Main> 
*Main> rows2col table 1
[150.697,179.323,203.212,226.505,249.633]
*Main> 

*Main> let x = rows2col table 0
*Main> let y = rows2col table 1
*Main> 
*Main> x
[1950.0,1960.0,1970.0,1980.0,1990.0]
*Main> y
[150.697,179.323,203.212,226.505,249.633]
*Main> 

*Main> let cols = rows2col table
*Main> 
*Main> cols 0
[1950.0,1960.0,1970.0,1980.0,1990.0]
*Main> cols 1
[150.697,179.323,203.212,226.505,249.633]
*Main> 


-}

table = [[1950, 150.697], [1960, 179.323], [1970,  203.212], [1980,  226.505], [1990,  249.633]]


rows2col rows idx = table |>> (\row -> row !! idx)

enumerate lst = zip [0..(length lst)] lst 

{- Find element in a list that satisfy a predicate -}
findIdxEl :: (Double -> Bool) -> t -> Maybe (Int, Double)
findIdxEl predicate lst = tuple
    where
    lst =  takeWhile (\row -> predicate $ snd row) (enumerate x)
    tuple = if null lst then Nothing else Just (last lst)

-- interpol :: Fractional a => [[a]] -> a -> a
interpol xyrows x = y
    where
    xcol = rows2col 0 xyrows
    ycol = rows2col 1 xyrows
    
    mx1 = fromJust $ findIdxEl (\a -> a <= x) xcol
    
    idx1 = fst mx1
    x1   = snd mx1
    x2   = xcol !! (idx1 + 1)
    
    y1   = ycol !! idx1
    y2   = ycol !! (idx1 + 1)
    
    y = (y2-y1)/(x2-x1)*(x-x1) + y1
    
    

    
    
    
    

x = rows2col table 0
y = rows2col table 1

