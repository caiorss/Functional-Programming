{-
   Old Control.Monad.State implementation that is curently outdated, 
however it provides backward compatibility to compile and run many State
Monad tutorials available on the internet and old books.


-}

module OldState where

{- runState :: State s a -> s -> (a, s) -}
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return a        = State $ \s -> (a,s)
  
  (State x) >>= f = State $ \s ->
    let (v,s') = x s
    in runState (f v) s'

instance Functor (State s) where
    fmap f (State g) = 
        State (\st -> let (a,  snext) = g st 
                       in (f a, snext))

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
execState :: State s a -> s -> s
execState act = snd . runState act


{-
    State Sequence Combinators:
    
The functions below are not defined in the old Control.State.Monad
library.

They make easier to compute the sucessive executions of the state
function.
-}

evalStateNtimes stateFunc state n =
    if  n == 0 
        then []
    else
        let (out, new_state) = runState stateFunc state in
        out:(evalStateNtimes stateFunc new_state (n-1))

runStateNtimes stateFunc state n =
    if  n == 0 
        then []
    else
        let (out, new_state) = runState stateFunc state in
        (out, new_state):(runStateNtimes stateFunc new_state (n-1))
        
execStateNtimes stateFunc state n =
    if  n == 0 
        then []
    else
        let (out, new_state) = runState stateFunc state in
        new_state:(execStateNtimes stateFunc new_state (n-1))


evalStateLoop stateFunc state =
    let (out, new_state) = runState stateFunc state in
    out:(evalStateLoop stateFunc new_state )

runStateLoop stateFunc state =
    let (out, new_state) = runState stateFunc state in
    (out, new_state):(runStateLoop stateFunc new_state )
    
execStateLoop stateFunc state =
    let (out, new_state) = runState stateFunc state in
    new_state:(execStateLoop stateFunc new_state )

evalNthState stateFunc state n =
    if  n == 0 
        then out
    else        
        evalNthState stateFunc new_state (n-1)
    where
        (out, new_state) = runState stateFunc state

 
  
