{-
 
http://learnyouahaskell.com/making-our-own-types-and-typeclasses

> people !! 0
Person {firstName = "Ayn", lastName = "Rand", age = 50}
> people !! 1
Person {firstName = "John", lastName = "Galt", age = 28}
> people !! 2
Person {firstName = "Adam", lastName = "Smith", age = 70}


> "person 0 is " ++ show (people !! 0)
"person 0 is Person {firstName = \"Ayn\", lastName = \"Rand\", age = 50}"
> 
> "person 1 is " ++ show (people !! 1)
"person 1 is Person {firstName = \"John\", lastName = \"Galt\", age = 28}"
> 
> "person 3 is " ++ show (people !! 2)
"person 3 is Person {firstName = \"Adam\", lastName = \"Smith\", age = 70}"
> 
> "person 2 is " ++ show (people !! 2)
"person 2 is Person {firstName = \"Adam\", lastName = \"Smith\", age = 70}


> let person = read "Person {firstName =\"Elmo\", lastName =\"NA\", age = 0}" :: Person
> person
Person {firstName = "Elmo", lastName = "NA", age = 0}
>

> let p  = people !! 1
> 
> p
Person {firstName = "John", lastName = "Galt", age = 28}
> 
> firstName p
"John"
> lastName p
"Galt"
> age p
28
> 
> 

> 
> ::t firstName 
> String
> 
> ::t lastName 
> String
> 
> ::t age 
> Int
>


*Main> map  showPerson people 
["Name: \"Ayn\" - Last Name: \"Rand\" - Age 50","Name: \"John\" - Last Name: \"Galt\" - Age 28","Name: \"Adam\" - Last Name: \"Smith\" - Age 70"]

 
-}

-- type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]


data Person = Person { firstName :: String, 
                       lastName :: String, 
                       age :: Int 
                     }
                     deriving (Eq, Show, Read)


people = [ Person { firstName = "Ayn",  lastName = "Rand",  age =50},
           Person { firstName = "John", lastName = "Galt",  age =28},
           Person { firstName = "Adam", lastName = "Smith", age =70}]

-- Get someone from the people database
getPerson n = people !! n

-- Show Person
showPerson :: Person -> String
showPerson person  = "Name: " ++ show(firstName person) ++ " - Last Name: " ++ show(lastName person) ++ " - Age " ++ show(age person)  

-- getName :: Person -> String
-- getName person = 


data Configuration = Configuration  String     -- User Name
                                     String    --  Local Host
                                     String     -- Remote Host
                                     Bool       -- Is guest
                                     deriving (Show)

username Configuration  x _ _ _ _  = x



phoneBook :: [(String, String)] 
phoneBook =    
    [("betty","555-2938")   
    ,("bonnie","452-2928")   
    ,("patsy","493-2928")   
    ,("lucille","205-2928")   
    ,("wendy","939-8282")   
    ,("penny","853-2492")   
    ]

