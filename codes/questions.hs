
questions = do
    putStrLn "\nWhats your name ??"
    name <- getLine
    
    putStrLn "\nWhere you come from ??"
    country <- getLine
    
    putStrLn "\nWhats your age ??"
    age <- getLine
    
    
    let result = "Your name is : " ++ name ++ "\nYou come from " ++ country  ++ "\nYour age is : " ++ age
    putStrLn result    
    

reverseInput = do 
    putStrLn "Enter a line of text:"
    x <- getLine
    putStrLn (reverse x)
