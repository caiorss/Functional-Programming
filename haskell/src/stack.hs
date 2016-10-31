import OldState

type Stack = [Int]

pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs) 



stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop  

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()  
