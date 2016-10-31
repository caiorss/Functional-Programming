

addMaybe1 :: Maybe Int 
addMaybe1 = 
    Just 4 >>= \x ->
    Just 5 >>= \y ->
    return (x+y)
            

addMaybe2 :: Maybe Int
addMaybe2 = do
    x <- Just 4
    y <- Just 5
    return (x+y)
    
    
profileSetup :: IO String
profileSetup = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "What is your game?"
    game <- getLine
    return (name ++ "'s name & " ++ game ++ "'s my game") 

profileSetup' :: IO String
profileSetup' = do
    let q1 =  "What is your name?"
    let q2 = "What is your game?"
    putStrLn q1
    name <- getLine   
    putStrLn q2
    game <- getLine
    return (name ++ "'s name & " ++ game ++ "'s my game") 

{-

Bind Operator

    f :: a -> M b
    bind (M a ) f = M b



λ> 
λ> (Just 2) `bind` (f 10) `bind` (f 0)
Nothing
λ> (Just 2) `bind` (f 0) `bind` (f 3)
Nothing
λ> 

-}

bind Nothing  f = Nothing
bind (Just x) f = 
    case (f x) of
        Nothing -> Nothing
        Just a  -> Just a

-- findCustomer :: (Eq a, Num a) => a -> Either [Char] [Char]
findCustomerByName :: Num b => [Char] -> Either [Char] b
findCustomerByName  "John Galt"           = Right 0
findCustomerByName  "Ayn Rand"            = Right 1
findCustomerByName  "Abrahan Lincol"      = Right 2
findCustomerByName  "James Madison"       = Right 3
findCustomerByName  _                     = Left  "Customer Not Found"

findCustomerByID :: (Eq a, Num a) => a -> Either [Char] [Char]
findCustomerByID  0 = Right "John Galt"
findCustomerByID  1 = Right "Ayn Rand"
findCustomerByID  2 = Right "Abrahan Lincol"
findCustomerByID  3 = Right "James Madison"
findCustomerByID  4 = Right "Richard Feyman"
findCustomerByID  _ = Left  "Customer ID doesn't exist"

findOrder 0 = Right (1,  "Pizza")
findOrder 1 = Right (2,  "Cake")
findOrder 2 = Right (1,  "Cake")
findOrder 3 = Right (3,  "Donuts")
findOrder 4 = Right (0,  "Ice Cream")
findOrder 5 = Right (0,  "Soda Pop")
findOrder 6 = Right (10,  "Soda Pop")
findOrder _ = Left "Order not Found"

greetCustomer :: (Eq a, Num a) => a -> IO ()
greetCustomer customerID =
    case findCustomerByID customerID of
        Right customer      -> print ("Hello " ++ customer)
        Left  errorMessage  -> print ("Erro: " ++ errorMessage)

