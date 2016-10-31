{-  http://haskell.1045720.n5.nabble.com/Stack-ADT-td3116658.html


-}

newtype Stack a = Stack [a]

emptyStack = Stack []

isEmptyStack (Stack xs) = null xs

push x (Stack xs) = Stack (x:xs)

pop (Stack (_:xs)) = Stack xs

top (Stack (x:_)) = x
