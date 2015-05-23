{-
   Old Control.Monad.State implementation that is curently outdated, 
however it provides backward compatibility to compile and run many State
Monad tutorials available in the internet.


-}
module OldState where

{- runState :: State s a -> s -> (a, s) -}
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return a        = State $ \s -> (a,s)
  (State x) >>= f = State $ \s ->
    let (v,s') = x s
    in runState (f v) s'

{- 
    Get: Set the result value to the state and left it unchanged
    get :: s -> (s, s)
-}    
get :: State s s 
get = State $ \s -> (s,s) 

{-    
Put set the result value to the state and leave the state 
unchanged.

    State s a ==  s -> (a,s) 

        put :: s -> State s ()
    Means
        put :: s -> s -> ((), s)

-}
put :: s -> State s ()
put newState = State $ \s -> ((),newState)     

{-  Upadate the State -}
modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

{- Returns the final value of computation -}
evalState :: State s a -> s -> a
evalState act = fst . runState act

{- Returns the final state of computation -}
exeCstate :: State s a -> s -> s
exeCstate act = snd . runState act
