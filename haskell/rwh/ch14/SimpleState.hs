
-- file: ch14/SimpleState.hs

{-
   This function transforms one state into another yielding
   a result (output). The state monad is also called
   State Transformer Monad. 

   s            : Type of state
   a            : Type of state output

   s -> (a, s)  : State transformer function 


            :: SimpleState s a         :: SimpleState s a

             |-------------|            |-------------|
State 0      |             | State 1    |             | State 2
        ---> | a -> (a, s) | ------>    | a -> (a, s) | ------->            
             |-------------|            |-------------|  
                  |                                |
                  |                                |
                 \ / Output 1: a                  \ /  Output 2: a

 -}
type SimpleState s a = s -> (a, s)

-- A type can be partially applied. The type constructor is:
-- SimpleState s
-- 
type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)

bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m fn = \s -> let (a, s') = m s
                    in (fn a) s'

{-
  A more readable version of bindSt 

   -- m == step
   -- k == makeStep
   -- s == oldState 
-}
bindAlt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindAlt step makeStep oldState =
  let (result, newState) = step oldState
  in (makeStep result) newState


{- Get current state and returens it as result -}
getSt :: SimpleState s s 
getSt = \s -> (s, s)

{- Set current state and ignore the current one. -}
putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
