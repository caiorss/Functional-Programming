{-
Credits: http://learnyouahaskell.com/for-a-few-monads-more

-}

-- import Control.Monad.State
import OldState
import System.Random 

{-
    The random function from System.Random has the following type:
    random :: (RandomGen g, Random a) => g -> (a, g)  
-}


randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  

{-
    ThreeCoins is now a stateful computations and after taking an initial 
    random generator, it passes it to the first randomSt, which produces 
    a number and a new generator, which gets passed to the next one and so on. 
    We use return (a,b,c) to present (a,b,c) as the result without changing 
    the most recent generator. Let's give this a go: 

-}
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c) 
