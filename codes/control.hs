{-
    This file contains pattern matching examples and control structure.

    https://www.haskell.org/tutorial/patterns.html
    http://learnyouahaskell.com/syntax-in-functions 

-}

f x y | y > z  = x^^2 - 10.5
      | y == z = x+10*y
      | y < z  = x/z + y
      where z = x^2 - 5*y

{-
-- passowrd :: (Integral a) => a -> String
password :: (Eq a, Num a) => a -> [Char]

    *Main> password 10
    "Error: Wrong Password pal"
    *Main> password 11
    "Error: Wrong Password pal"
    *Main> password 3423
    "OK - Safe opened"
    *Main> 

-}
password :: (Eq a, Num a) => a -> [Char]
password 3423 = "OK - Safe opened"
password x    = "Error: Wrong Password pal"



{-
    *Main> map sayMe [1..8]
    ["One!","Two!","Three!","Four!","Five!","Not between 1 and 5",
    "Not between 1 and 5","Not between 1 and 5"]

-}
sayMe :: (Integral a) => a -> [Char]
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

{-
 
> map charName ['a', 'b', 'c', 'z' ]
["Albert","Brown","Cecil","Zoe"]
 
> charName 'd'
"*** Exception: control.hs:(47,1)-(50,20): Non-exhaustive patterns in function charName


-}
charName :: Char -> [Char]
charName 'a' = "Albert"
charName 'b' = "Brown"
charName 'c' = "Cecil"
charName 'z' = "Zoe"


{-

    λ 
    λ map move ['w', 's', 'a', 'd', 'f', 'k']
    ["UP!","DOWN!","LEFT!","RIGHT!","HANG AROUND AND DO NOTHING!","HANG AROUND AND DO NOTHING!"]
    λ 

 -}
move :: Char -> [Char]
move a
    |  a == 'w'       = "UP!"
    |  a == 's'       = "DOWN!"
    |  a == 'a'       = "LEFT!"
    |  a == 'd'       = "RIGHT!"
    |  otherwise      = "HANG AROUND AND DO NOTHING!"

 
--
--  PATTERNS WITH TUPLES --
--
{-
 
    *Main> addVectors (8, 9)(-10, 12)
    (-2,21)
    *Main> 
    *Main> addv1 = addVectors (1, 3)

    <interactive>:26:7: parse error on input `='
    *Main> 
    *Main> let addv1 = addVectors (1, 3)
    *Main> 
    *Main> map addv1 [(12, 23), (45, 23), (6, 14)]
    [(13,26),(46,26),(7,17)]
    
 
  -}
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

--addVectors2 :: (Num t, Num t1) => (t, t1) -> (t, t1) -> (t, t1) 
{-
 
    *Main> addVectors (8, 9)(-10, 12)
    (-2,21)
    *Main> addVectors2 (8, 9)(-10, 12)
    (-2,21)
 -}
addVectors2 (x1, y1) (x2, y2) = (x1+x2, y1+y2) 


{-

    *Main> :t add3Dvectors 
    add3Dvectors
    :: (Num t, Num t1, Num t2) =>
        (t, t1, t2) -> (t, t1, t2) -> (t, t1, t2)


    * Main> add3Dvectors (23, 12, 233) (10, 100, 30)
    (33,112,263)

 
  -}
add3Dvectors (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)


--
--  3D tuple constructors 
--  ----------------------------
{-
 
    *Main> :t first
    first :: (t, t1, t2) -> t
    *Main> 
    *Main> :t second
    second :: (t, t1, t2) -> t1
    *Main> 
    *Main> :t third
    third :: (t, t1, t2) -> t2
     
    > first (1, 2, 3)
    1
    > second  (1, 2, 3)
    2
    > third (1, 2, 3)
    3   

-}

first  (x, _, _) = x
second (_, y, _) = y
third  (_, _, z) = z

{-
    *Main> sum' [1, 2, 3, 4, 5]
    15
-}
sum' []     = 0
sum' (x:xs)  = x + sum' xs


{-

    *Main> :t cylinder_area 
    cylinder_area :: Floating t => t -> t -> (t, t)
    *Main> 
    *Main> cylinder_area 12 10
    (753.9822368615503,452.3893421169302)


-}
cylinder_area r h = (sideArea, topArea)
                  where 
                  sideArea = 2*pi*r*h
                  topArea  = pi*r^2


{-
-------------------------------------

    *Main> describeList [1]
    "The list isa singleton List"

    Main> describeList []
    "The list isempty"

    *Main> describeList [1..3]
    "The list isA longer list"

-------------------------------------
-}

describeList xs = "The list is" ++ what xs
                where 
                what [] = "empty"
                what [x] = "a singleton List"
                what xs  = "A longer list"

{-
 
    *Main> 
    *Main> listCase []
    "The list is empty list"
    *Main> 
    *Main> listCase ['a']
    "The list is singleton listy"
    *Main> 
    *Main> listCase [1, 2, 3]
    "The list is stronger list"
    *Main> 

  -}
listCase :: [a] -> String
listCase xs = "The list is " ++ case xs of 
                                        []  -> "empty list"
                                        [x] -> "singleton listy"
                                        xs  -> "stronger list"


