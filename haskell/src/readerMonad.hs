newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
    -- 
    -- fmap is the function compsition between function fn and
    -- the function wrapped by the reader monad.
    --
    -- fmap :: (a -> b) -> (Reader r) a -> (Reader r) b 
    --
    fmap fn (Reader rToA) = Reader $ \ r -> fn $ rToA r



instance Applicative (Reader r) where    
    {- | 
        pure :: Applicative f => a -> f a
        pure :: a -> (Reader r) a
    -}
    pure a = Reader $ \_ -> a

    {- | (<*>) :: Applicative f => f (a -> b) -> f a -> f b
         (<*>) :: Reader r (a -> b) -> (Reader r) a -> (Reader r) b
    -}
    (Reader fn) <*> (Reader rToA) =
        Reader $ (\r -> let a     = rToA r in
                        let fAtoB = fn r   in
                        fAtoB a
                 )

instance Monad (Reader r) where
    
    {- | return :: a -> (Reader r) a  -}
    return a = Reader $ \ _ -> a

    {- |

      (>>=) :: (Monad m) => m a -> (a -> m b) -> m b 

      (Reader r) a >>= (a -> (Reader r) b) -> (Reader r) b
    -}
    (Reader rToA) >>= fn =
        Reader $ \ r -> let a = rToA r
                        in  runReader (fn a) r 


{- | Read environment, aka configuration. -}
ask :: Reader r r
ask = Reader (\ r -> r)


local :: (r -> r) -> Reader r a -> Reader r a
local fn (Reader rToA) = Reader $ \ r -> rToA (fn r)         
