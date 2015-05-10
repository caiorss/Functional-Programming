## Abstract Data Type

**Example: Days of Week**

Enumerated sets is type which can only have a limited number of values. 


```haskell

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Eq, Ord, Enum)

fromDay :: Weekday -> Int
fromDay = fromEnum

toDay :: Int -> Weekday
toDay = toEnum   

> map (Monday<) [ Tuesday, Friday, Sunday]
[True,True,True]

> map (Thursday<) [Monday, Tuesday, Friday, Sunday]
[False,False,True,True]

> 
> Monday == Tuesday
False
> Tuesday == Tuesday
True
>  

> 
> fromDay Saturday 
5

> 1 + fromDay Monday 
1
> 1 + fromDay Saturday 
6
> Saturday 
Saturday
> 
> toDay 0
Monday
> toDay 6
Sunday
> 
> 


```

**Example: Colors**

```haskell

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
colorToRGB (CustomColor r g b) = (r,g,b)   -- this one is new
 
    
> 
> Red == White
False
> 
> Red == Red
True
> 

> let b = CustomColor 120 240 100
> colorToRGB b
(120,240,100)

> map colorToRGB [ Blue, White, Yellow ]
[(0,0,255),(255,255,255),(255,255,0)]
> 


```

**Example: Shapes**

```haskell

data Shape = Circle  Float 
            | Rect   Float Float 

square   :: Float -> Shape
square n =  Rect n n 

area            :: Shape -> Float
area (Circle r)  = pi * r^2
area (Rect x y)  = x  * y

*Main> area $  Rect 20 30
600.0
*Main> area $ Circle 20
1256.6371
*Main> area $ square 20
400.0
*Main> 


```

**Example: Students GPA**

```haskell

data Student = USU String Float 
             deriving (Show)

get_gpa :: Student -> Float
get_gpa (USU _ grade) = grade

get_name :: Student -> String
get_name (USU name _ ) = name

class_gpa :: [Student] -> Float
class_gpa myclass = (sum c) / fromIntegral  (length c)
                  where 
                  c = map get_gpa myclass


*Main> let myke = USU "Mike" 4.0
*Main> 
*Main> get_name myke
"Mike"
*Main> get_gpa myke
4.0
*Main> 

*Main> let myclass = [USU "Mike" 3.7, USU "Steve" 3.9, USU "Fred" 2.9, USU "Joe" 1.5]
*Main> 

*Main> class_gpa myclass 
3.0
*Main

```

**Example: Typeclass with record Syntax**

```haskell

data Person = Person { firstName :: String, 
                       lastName :: String, 
                       age :: Int 
                     }
                     deriving (Eq, Show, Read)


people = [ Person { firstName = "Ayn",  lastName = "Rand",  age =50},
           Person { firstName = "John", lastName = "Galt",  age =28},
           Person { firstName = "Adam", lastName = "Smith", age =70}]


{- Get someone from the people database -}
getPerson n = people !! n

{- Show Person -}
showPerson :: Person -> String
showPerson person  = "Name: " ++ show(firstName person) ++ " - Last Name: " ++ show(lastName person) ++ " - Age " ++ show(age person) 

> people
[Person {firstName = "Ayn", lastName = "Rand", age = 50},Person {firstName = "John", lastName = "Galt", age = 28},Person {firstName = "Adam", lastName = "Smith", age = 70}]
> 

> map firstName people
["Ayn","John","Adam"]
> 

> map (\el ->  fst el ++ " " ++  snd el) $ zip (map firstName people) (map lastName people)
>  ["Ayn Rand","John Galt","Adam Smith"]


> people !! 1
Person {firstName = "John", lastName = "Galt", age = 28}
> people !! 2
Person {firstName = "Adam", lastName = "Smith", age = 70}
> 

> "person 0 is " ++ show (people !! 0)
"person 0 is Person {firstName = \"Ayn\", lastName = \"Rand\", age = 50}"
> 
> "person 1 is " ++ show (people !! 1)
"person 1 is Person {firstName = \"John\", lastName = \"Galt\", age = 28}"
>


> let person = read "Person {firstName =\"Elmo\", lastName =\"NA\", age = 0}" :: Person
> person
Person {firstName = "Elmo", lastName = "NA", age = 0}
> 
> firstName person 
"Elmo"
> last
last      lastName
> lastName person 
"NA"
> age person
0
> 

> let tesla = Person { firstName = "Nikola", lastName = "Tesla", age =30}
> tesla
Person {firstName = "Nikola", lastName = "Tesla", age = 30}
> 

> showPerson tesla
"Name: \"Nikola\" - Last Name: \"Tesla\" - Age 30"
> 

```

Reference: 
    * http://learnyouahaskell.com/making-our-own-types-and-typeclasses


