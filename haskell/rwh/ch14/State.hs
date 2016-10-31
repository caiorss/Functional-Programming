
{-
  Applies a state transformer to a state and returns a new state
  yielding a result. 
  
  runState :: State s a -> s -> (a, s)

 -}
newtype State s a = State { runState :: s -> (a, s)  }


returnState :: a -> State s a
returnState a = State ( \s -> (a, s) )

bindState :: State s a -> (a -> State s b) -> State s b
bindState m fn = State $ \s -> let (a, s') = runState m s
                               in runState (fn a) s'

-- evalState : Returns only the result, throwing away the final state
--
evalState :: State s a -> s -> a
evalState fn s = fst (runState fn s)

-- execState : Throws the result away, returning only the final state
execState :: State s a -> s -> s
execState fn s = snd (runState fn s)


get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\ _ -> ((), s))

{- State Monad Evaluation Functions -}

-- runState : Returns both the result and the final state



{- State Monad Evaluation Functions -}

-- runState : Returns both the result and the final state



{-
   Applies a function to the result of the
   state transformer (state monad) application
   keeping the current state. 

-}
instance Functor (State s) where
  
  {- fmap :: (a -> b) -> F a -> F b -}
  --
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f fns =
    State $ \oldState -> let (output, newState) = runState fns oldState
                         in (f output, newState)
                 


instance Applicative (State s) where
  
  pure = returnState

  --
  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  --
  -- fsa :: State s a
  --
  -- fn :: State s (a -> b)
  --
  -- output :: a 
  -- newState :: s
  --
  -- f_a_to_b :: a -> b
  -- newState' :: s
  --
  fn <*> fsa = State $ \ oldState ->
    let (output, newState) = runState fsa oldState in
    let (f_a_to_b, newState') = runState fn newState in
    (f_a_to_b output, newState')


               

instance Monad (State s) where

  -- return :: a -> State s a 
  --
  return a = State $ \s ->  (a, s)   
  
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  --
  -- StateFn    :: State s a
  --
  -- stateMaker :: a -> State s b 
  --
  -- result     :: a
  --
  -- newState   :: s 
  --
  --  
  --
  stateFn >>= stateMaker  =

    State $ \oldState -> let (result, newState) = runState stateFn oldState
                         in  runState (stateMaker result) newState

                             
