--
--  The Finite State-ness of FSM-Hume 
--  Greg Michaelson 1 , Kevin Hammond 2 and Jocelyn Serot 3
--
--

newtype State s a = State { runState :: s -> (a, s) }

data STATE = ZERO | ONE     deriving (Eq, Show, Read)
data OUTPUT = OK | ERROR    deriving (Eq, Show, Read)
type INPUT =  Int


evalState f s = fst $ runState f s
execState f s = snd $ runState f s

get   = State $ \s -> (s,  s)
put s = State $ \_ -> ((), s)

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k  = State $ \s ->
    let (a, s') = runState m s
    in  runState (k a) s'


trans (ZERO, 0) = (ZERO, ERROR)
trans (ZERO, 1) = (ONE,  OK)
trans (ONE,  0) = (ZERO, ERROR)
trans (ONE,  1) = (ONE,  ERROR)

pipe_trans :: (STATE, OUTPUT) -> INPUT -> (STATE, OUTPUT)
pipe_trans (state, output) input = trans (state, input)


state0 = ZERO
inputs = [0, 1, 0, 1, 1, 0, 0]


f i = do
    (s, o) <- get
    let (snew, outn) = trans (s, i)
    put (s, 0)
    return snew


main = do

    putStrLn "Type the input : "
    input <- getLine
    putStrLn input
    main
