

data Lens s a = Lens { set :: s -> a -> s,
                        view :: s -> a
                      }

view :: Lens s a -> s -> a
set  :: Lens s a -> s -> a -> s

