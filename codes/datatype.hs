{-
http://www.scs.stanford.edu/11au-cs240h/notes/
http://www.scs.stanford.edu/14sp-cs240h/

http://aprendehaskell.es/content/OrdenSuperior.html

http://channel9.msdn.com/Series/C9-Lectures-Erik-Meijer-Functional-Programming-Fundamentals/C9-Lectures-Dr-Erik-Meijer-Functional-Programming-Fundamentals-Chapter-10-of-13

http://www.cs.bham.ac.uk/~vxs/teaching/Haskell/handouts/basics.pdf

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




data Student = USU String Float 
             deriving (Show)
             

myclass = [USU "Mike" 3.7, USU "Steve" 3.9, USU "Fred" 2.9, USU "Joe" 1.5]


get_gpa :: Student -> Float
get_gpa (USU _ grade) = grade

get_name :: Student -> String
get_name (USU name _ ) = name

class_gpa :: [Student] -> Float
class_gpa myclass = (sum c) / fromIntegral  (length c)
                  where 
                  c = map get_gpa myclass

mike = USU "Mike" 3.7


data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Eq, Ord, Enum, Show)
           
fromDay :: Weekday -> Int
fromDay = fromEnum

toDay :: Int -> Weekday
toDay = toEnum   



data Color
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | White
    | Black
    | CustomColor Int Int Int -- R G B components
    deriving (Eq)

colorToRGB Red    = (255,0,0)
colorToRGB Orange = (255,128,0)
colorToRGB Yellow = (255,255,0)
colorToRGB Green  = (0,255,0)
colorToRGB Blue   = (0,0,255)
colorToRGB Purple = (255,0,255)
colorToRGB White = (255,255,255)
colorToRGB Black = (0,0,0)
colorToRGB (CustomColor r g b) = (r,g,b)   -- this one is 



-- file: ch03/BookStore.hs
data Customer = Customer {
      customerID        :: CustomerID
    , customerName      :: String
    , customerAddress   :: Address
} deriving (Show)
