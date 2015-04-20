{-

http://rosettacode.org/wiki/Euler_method

Solve differential equation by the Euler's Method.

    T(t)
    ---- =  -k(T(t) - Tr)
     dt
    
    T(t) = Tr + k(T0(t) - Tr)exp(-k*t)

Task

The task is to implement a routine of Euler's method and then to use it to solve the given example of Newton's cooling law with it for three different step sizes of 2 s, 5 s and 10 s and to compare with the analytical solution. The initial temperature T0 shall be 100 °C, the room temperature TR 20 °C, and the cooling constant k 0.07. The time interval to calculate shall be from 0 s to 100 s.

A reference solution (Common Lisp) can be seen below. We see that bigger step sizes lead to reduced approximation accuracy. 


*Main> maxerror (map snd xy1) (map snd xy2)
1.0613728929193513
*Main> 
*Main> maxerror (map snd xy1) (map snd xy3)
6.241274732587954e-6

    
-}


import Graphics.Gnuplot.Simple




slice lst a b =  [ lst !! i | i <- [0..(length lst)], a <= i && i <= b]


-- step dt t x v = (t', v', 'a) =


-- eulerstep :: Num t => ((t, t) -> t) -> t -> (t, t) -> (t, t)
eulerStep f step (x, y)= (xnew, ynew)
                    where
                    xnew = x + step
                    ynew = y + step * (f (x, y))

--euler :: (Floating t, Ord t) => ((t, t) -> t) -> t -> t -> t -> t -> [(t, t)]
euler :: ((Double, Double) -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
euler f x0 xf y0 step = xypairs
                     where
                     iterator = iterate $ eulerStep f step
                     xypairs = takeWhile (\(x, y) -> x <= xf ) $ iterator (x0, y0)


rk4Step f h (x, y) = (xnext, ynext)
                      where
                      
                      k1 = f (x, y)
                      k2 = f (x+h/2, y+h/2*k1)
                      k3 = f (x+h/2, y+h/2*k2)
                      k4 = f (x+h,y+h*k3)
                      
                      xnext = x + h
                      ynext = y + h/6*(k1+2*k2+2*k3+k4)
                      
--rk4 :: (Floating t, Ord t) => ((t, t) -> t) -> t -> t -> t -> t -> [(t, t)]
rk4 :: ((Double, Double) -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
rk4 f x0 xf y0 h = xypairs
                     where
                     iterator = iterate $ rk4Step f h
                     xypairs = takeWhile (\(x, y) -> x <= xf ) $ iterator (x0, y0)

-- Comparing Floating Point Number
equalflt_accuracy accuracy num1 num2 = abs(num1 - num2) < 10^^(-accuracy)


{-
    Map a list of functions to a single element
    *Main> let f1 x = x^2 - 10.5
    *Main> let f2 x = 8.5*10 - 5
    *Main> let f3 x = 10/x + 5
    *Main> 
    
    *Main> joinf [f1, f2, f3] 4
    [5.5,80.0,7.5]


-}
joinf functions element = map ($ element) functions

mapzip func list =   zip list (map func list)

--tabulatef :: (Double -> Double)
tabulatef func list =   zip list (map func list)

range start end step = [start,step..end]

temp t = 20.0 + 80.0*exp(-0.07*t)


--f :: (Double, Double) -> Double
--f :: Floating a => (a, a) -> a
f :: (Double, Double) -> Double
f (t, temp) = -0.07*(temp-20.0)


xy1 = mapzip temp (range 0 100 5) :: [(Double, Double)]
xy2 = euler f 0.0 100.0 100.0 5
xy3 = rk4   f 0.0 100.0 100.0 5




maxerror y1 y2 = maximum $ zipWith (\y2 y1 -> abs(y2-y1))  y2 y1



quicksort  []     =  []
quicksort (x:xs)  =  quicksort [y | y <- xs, y<x ]
                  ++ [x]
                  ++ quicksort [y | y <- xs, y>=x]


pairs xs = zip xs (tail xs)


interpolate xytable x =  y
      where
      xybounds = last $  takeWhile (\(t1, t2) -> (fst t1) <= x ) (pairs xytable)
      x1 = fst $ fst xybounds
      x2 = fst $ snd xybounds
      y1 = snd $ fst xybounds
      y2 = snd $ snd xybounds
      
      y = (y2-y1)/(x2-x1)*(x-x1) + y1
                      
                      

ftest x = x^^2 - 10*x +8
lut = mapzip ftest  (range 0 10.0 2)
ftest' = interpolate lut
fterrror x = abs $ ftest x - ftest' x

ft = joinf [ftest, ftest', fterrror]

-- Impura Functions
hello :: String -> IO()
hello str = putStrLn ("Hello " ++ str)


-- IO Monad
nextSum :: Int -> IO Int
nextSum i = do
    putStrLn ("Successor of " ++ show i)
    return (succ i)

encrypt :: String -> String
encrypt a = map succ a


user_input = do
    input <- getLine
    putStrLn (encrypt input)

--plotpairs :: IO()
-- plotpairs =  plotList [] xy1

fat :: Integral t=> t -> t
fat 0  = 1
fat 1  = 1
fat n = n*fat(n-1)

factorialIO :: IO ()
factorialIO = do 
            putStr "Type a number :"
            num <- getLine
            putStr "The result is :"
            print (fat(read num))


--main :: IO() 
main = do
    plotList [] xy1
    plotList [] xy2
    plotList [] xy3
