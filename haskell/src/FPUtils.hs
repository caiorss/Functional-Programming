module FPUtils where

{- Operators -}
(|>) x f    = f x       -- Piping
(>>) f g x  = g (f x)   -- Composition


{- Iterators -}
pairs alist = zip alist (tail alist)
triples alist = zip3 alist (tail alist) (tail $ tail alist)
sliding n alist = map (take n) (take (length(alist) -n + 1 ) $ iterate tail alist)
enumerate alist = zip [0..(length(alist)-1)] alist

lagdiff alist = map (uncurry ( flip (-))) $ pairs alist

{- Multiple Functions Applications -}
juxt fs x = map ($ x) fs
juxt2 (f1, f2) x = (f1 x, f2 x)
juxt3 (f1, f2, f3) x = (f1 x, f2 x, f3 x)
juxt4 (f1, f2, f3, f4) x = (f1 x, f2 x, f3 x, f4 x)
juxt5 (f1, f2, f3, f4, f5) x = (f1 x, f2 x, f3 x, f4 x, f5 x)

{- Control Flow -}
ifelse   pred a  b   x  =  if pred x then a    else b
ifelseEq pred a      x  =  if pred x then a    else x
ifelseDo pred fa fb  x  =  if pred x then fa x else fb x

{- Tuples -}
swap (a, b) = (b, a)


uncurry3 f (a, b, c) = f a b c
uncurry4 f (a, b, c, d) = f a b c d
uncurry5 f (a, b, c, d, e) = f a b c d e

curry3 f a b c = f (a, b, c)
curry4 f a b c d = f (a, b, c, d)
curry5 f a b c d e = f (a, b, c, d, e)
