{-
http://channel9.msdn.com/Series/C9-Lectures-Erik-Meijer-Functional-Programming-Fundamentals/C9-Lectures-Dr-Erik-Meijer-Functional-Programming-Fundamentals-Chapter-10-of-13

Example:

    > 
    > area $  Rect 20 30
    600.0
    > 
    > area $ Circle 20
    1256.6371
    > 
    
    > area $ square 20
    400.0
    > 
    > 

-}
data Shape = Circle  Float 
            | Rect   Float Float 

square   :: Float -> Shape
square n =  Rect n n 

area            :: Shape -> Float
area (Circle r)  = pi * r^2
area (Rect x y)  = x  * y
