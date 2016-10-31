{-
 
*Main> 
*Main> vecf f (Scalar 10)
Scalar 100.0
*Main> 
*Main> vecf f (Array [1..10])
Array [91.0,84.0,79.0,76.0,75.0,76.0,79.0,84.0,91.0,100.0]
*Main> 
*Main> let fvectorized  = vecf f 
*Main> 
*Main> fvectorized  (Array [1..10])
Array [91.0,84.0,79.0,76.0,75.0,76.0,79.0,84.0,91.0,100.0]
*Main> 
*Main> fvectorized (Scalar 10)
Scalar 100.0
*Main> 
*Main> 

*Main> 
*Main> vectornorm (Array [-10, 20, 30])
37.416573867739416
*Main> 
*Main> vectornorm (Scalar 10)
10.0
*Main> 
*Main>

 
 -}

-- Pipe Operators
(|>) x f = f x
(|>>) x f = map f x
(?>>) x f = filter f x


data Vector  = Scalar Double |  Array [Double]
              deriving( Eq, Show)

-- Type constructors
--
scalar x = Scalar x
array v =  Array  v

-- Get a Double from type Vector(Scalar)
sValue :: Vector -> Double
sValue (Scalar x) = x

-- Get a Double array from Vector([Double])
sArray :: Vector -> [Double]
sArray (Array v) = v

f1 :: Vector -> Vector
f1 (Scalar x)  =  Scalar ( x^2 - 10*x + 100)

mapArray f lst  =  lst |>> scalar |>> f |>> sValue |> array

f x = x^2 - 10.0*x + 100.0

vecf f (Scalar x) = Scalar ( f x )
vecf f (Array  xs) = Array  ( map f xs)

vectornorm (Scalar x)  = abs(x)
vectornorm (Array xs)  = map (\x -> x^2) xs |> sum |> sqrt

