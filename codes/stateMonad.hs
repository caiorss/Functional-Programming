{-
http://heh.fi/state-monad

-}
import Control.Monad
import System.Random

newtype State s a = State { runState :: s -> (a, s) }

evalState f s = fst $ runState f s
execState f s = snd $ runState f s

get   = State $ \s -> (s,  s)
put s = State $ \_ -> ((), s)

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k  = State $ \s ->
    let (a, s') = runState m s
    in  runState (k a) s'

f :: State Integer [Char]
f = do
    s <- get
    put (s+1)
    return "foo"


example0 = runState f 42 where
  f = do
    s <- get
    put (s+1)
    return "foo"

-- randomR conveniently follows the ‘s -> (a, s)’ pattern.
die sides = State $ randomR (1, sides :: Integer)

n `dice` sides = sum `liftM` replicateM n (die sides)    
    
example1 = evalState f (mkStdGen 0) where
  f = do
    a <- die 6        -- throw 1d6
    b <- 2 `dice` 6   -- throw 2d6
    c <- 6 `dice` 10  -- throw 6d10
    return [a, b, c]
